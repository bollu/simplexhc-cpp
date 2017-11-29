#pragma once
#include <iostream>
#include <set>
#include <sstream>
#include "sxhc/types.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"


#define DEBUG_TYPE "stackMatcher"
using PushPopPair = std::pair<llvm::CallInst *, llvm::CallInst *>;


// We assume that both A and B have only inter-block push and pop pairs.
std::set<PushPopPair> getCommonAllowedPushPopPairs(std::set<PushPopPair> A, std::set<PushPopPair> B) {
    const std::set<llvm::CallInst *> commonPushes = [&] {
        std::set<llvm::CallInst *> apushes;

        for (PushPopPair pa: A) apushes.insert(pa.first);

        std::set<llvm::CallInst *> commonPushes;
        for(PushPopPair pb : B) {
            assert(pb.first != pb.second);
            if(apushes.count(pb.first)) {
                commonPushes.insert(pb.first);
            }

        }
        return commonPushes;
    }();

    std::set<PushPopPair> result;

    auto insertCommonPairs = [&] (const std::set<PushPopPair > &pairs, std::set<PushPopPair> &container) {
        std::set<PushPopPair> ps;
        for(PushPopPair p : pairs)
            if (commonPushes.count(p.first)) container.insert(p);

        return ps;
    };

    insertCommonPairs(A, result);
    insertCommonPairs(B, result);

    return result;

}

using namespace llvm;
// Pass to match abstract stack manipulations and eliminate them.
class StackMatcherPass : public PassInfoMixin<StackMatcherPass> {
public:
    explicit StackMatcherPass(StringRef stackname) : stackname(stackname) {}
    static StringRef name() { return "stackMatcher"; }

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {


        if (F.isDeclaration()) { return llvm::PreservedAnalyses::all(); }
        // if (F.getName() != "main") return llvm::PreservedAnalyses::all();

        DominatorTree &DT = FAM.getResult<DominatorTreeAnalysis>(F);
        assert(!F.isDeclaration() && "expected F to be a definition.");
        std::set<PushPopPair> inter, intra;

        std::tie(inter, intra) = visitBB(F.getEntryBlock(),
                                         std::stack<CallInst *>(),
                                         DT,
                                         std::set<BasicBlock *>());

        std::set<PushPopPair> replacements;
        replacements.insert(inter.begin(), inter.end());
        replacements.insert(intra.begin(), intra.end());


        for (PushPopPair r : replacements) {
            StackMatcherPass::propogatePushToPop(r);
        }

        /* first propogate then delete because multiple pushes can use the same pop.
         eg:
               A - push
              / \
         pop- B  C-pop
        
         We expect the push to be propogated to both B and C, and then we remove the push.
         */
        {
            std::set<CallInst *>pushes;
            for (PushPopPair r : replacements) {
                pushes.insert(r.first);

            }
            for(CallInst *push : pushes) { push->eraseFromParent(); }
        }

        return llvm::PreservedAnalyses::none();
    }

    void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.addRequired<DominatorTreeWrapperPass>();
    }


private:
    std::string stackname;

    using InterBlockPushPopPairs = std::set<PushPopPair>;
    using IntraBlockPushPopPairs = std::set<PushPopPair>;

    // intra = any set of push/pop that is below the current BB
    // inter = any push/pop that is mix of things below and above the
    // current BB.
    std::pair<IntraBlockPushPopPairs, InterBlockPushPopPairs> visitBB(BasicBlock &BB, std::stack<CallInst *> pushStack, const DominatorTree &DT, std::set<BasicBlock *> Visited) {

        Visited.insert(&BB);
        IntraBlockPushPopPairs intraBlockPushPopPairs;
        InterBlockPushPopPairs interBlockPushPopPairs;

        std::set<CallInst *> toDelete;

        for(Instruction &I : BB) {
            // We make _heavy_ assumptions about our IR: that is, that any instruction we don't understand can't hurt us
            // in particular, that nothing can interfere with push/pop.
            if (!isa<CallInst>(I)) continue;

            CallInst *CI = cast<CallInst>(&I);
            // indirect call. Do not try to analyze
            if (!CI->getCalledFunction()) continue;

            const std::string calleeName = CI->getCalledFunction()->getName();
            if (calleeName == "push" + stackname) {
                pushStack.push(CI);
                // dbgs() << "pushing: " << *CI << "\n";

            }
            else if (calleeName == "pop" + stackname) {
                // We do not have a matching push, our function is incomplete. continue
                if (pushStack.size() == 0) continue;
                CallInst *Push = pushStack.top();
                pushStack.pop();

                // intra block
                if(Push->getParent() == CI->getParent()) {
                    intraBlockPushPopPairs.insert(std::make_pair(Push, CI));
                }
                else {
                    interBlockPushPopPairs.insert(std::make_pair(Push, CI));
                }
            }
        }


        // If you are next in the CFG and are dominated in the DT, then you _will_ have the stack state your
        // parent has. We need both to be satisfied. (Why?)
        // Consider a CFG:
        //     A
        //   /   \
        //  B    C
        //  \   /
        //    D
        // Corresponding DT:
        //        A
        //      / | \
        //     B  D  C
        //
        // Just because A dom D, does not mean that D will use A's stack state.
        const TerminatorInst *TI = BB.getTerminator();

        // list of push/pop pairs of all children.
        std::vector<InterBlockPushPopPairs> childPpsList;

        for(unsigned i = 0; i < TI->getNumSuccessors(); i++) {
            BasicBlock *Next = TI->getSuccessor(i);
            if (!DT.dominates(&BB, Next)) continue;

            IntraBlockPushPopPairs curIntra;
            InterBlockPushPopPairs curInter;
            std::tie(curIntra, curInter) = visitBB(*Next, pushStack, DT, Visited);
            // whatever is intra to the child will definitely be intra to the parent.
            intraBlockPushPopPairs.insert(curIntra.begin(), curIntra.end());
            childPpsList.push_back(curInter);
        }

        // count the number of times a push is matched.
        std::map<CallInst *, unsigned> pushScoreboard;
        for(InterBlockPushPopPairs pps : childPpsList) {
            for(PushPopPair pp : pps)
                pushScoreboard[pp.first]++;
        }


        for(InterBlockPushPopPairs pps : childPpsList) {
            for(PushPopPair pp : pps) {
                assert(pushScoreboard.find(pp.first) != pushScoreboard.end());
                // all children access this push, then add this
                if (pushScoreboard[pp.first] == TI->getNumSuccessors()) {
                    if (pp.first->getParent() == &BB) {
                        intraBlockPushPopPairs.insert(pp);
                    } else {
                    interBlockPushPopPairs.insert(pp);
                    }
                }

            }
        }

        return std::make_pair(intraBlockPushPopPairs, InterBlockPushPopPairs()); // interBlockPushPopPairs);


    }

    static void propogatePushToPop (PushPopPair r) {
        CallInst *Push = r.first;
        CallInst *Pop = r.second;
        Value *PushedVal = Push->getArgOperand(0);
        BasicBlock::iterator ii(Pop);
        ReplaceInstWithValue(Pop->getParent()->getInstList(), ii, PushedVal);
    }

};

Instruction *getOwningInst(User *U) {
    if (isa<Instruction>(U)) return cast<Instruction>(U);
    if (Constant *C = dyn_cast<Constant>(U)) {
        assert(C->getNumUses() == 1 && "constant has more than one use");
        return getOwningInst(*C->users().begin());
    }

    errs() << "unknown value: " << *U << "\n";
    report_fatal_error("unreachable code in getOwningInst");
}



class EliminateUnusedAllocPass : public PassInfoMixin<EliminateUnusedAllocPass> {
public:

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {
        if (F.isDeclaration()) { return llvm::PreservedAnalyses::all(); }
        // hack.
        if (true){ return llvm::PreservedAnalyses::all(); }
        AAResults &AA = FAM.getResult<AAManager>(F);
        runInternal(F, AA);

        return llvm::PreservedAnalyses::none();
    }

    void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.addRequired<DominatorTreeWrapperPass>();
    }

private:

    // This instruction uses a pointer. Return if this instruction `I` allows
    // the pointer to escape.
    static bool isUserThatCreatesEscape(Instruction *I, Value *MaybeEscapingVal) {
        if (auto CI = dyn_cast<CallInst>(I)) {
            if (!CI->getCalledFunction())  { report_fatal_error ("indirect function call allows pointer to escape."); }
            if (CI->getName() == "pushReturn") return true;
            return false;
        }
        else if (auto SI = dyn_cast<StoreInst>(I)) {
            // store  <val> <maybeescapingval> does NOT escape.
            // store  <maybeescapingval> <escapeloc> DOES escape.
            return SI->getOperand(0) == MaybeEscapingVal;
        }
        else if (auto LI = dyn_cast<LoadInst>(I)) {
            return false;
        }
        else if (isa<BitCastInst>(I) || isa<GetElementPtrInst>(I)) {
            return doesEscape(I, MaybeEscapingVal);
        }
        errs() << "unknown instruction pass to " << __PRETTY_FUNCTION__ << ": " << *I << "\n";
        report_fatal_error("unknown instruction.");
    }

    static bool doesEscape(Instruction *I, Value *MaybeEscapingVal) {
        for(User *U : I->users()) {
            Instruction *User = getOwningInst(U);
            if (isUserThatCreatesEscape(User, MaybeEscapingVal)) return true;
        }
        return false;
    }

    void runInternal(Function &F, AAResults &AA){
        errs() << "Eliminate unused alloc running on: " << F.getName() << "\n";

        for(BasicBlock &BB : F) {
            for(Instruction &I : BB) {
                if(!isa<CallInst>(I)) continue;
                CallInst &CI = cast<CallInst>(I);
                // we don't analyse indirect calls.
                if(!CI.getCalledFunction()) continue;

                assert(CI.getCalledFunction() && "should have bailed out by this point at an indirect function");
                if (CI.getCalledFunction()->getName() != "alloc") {
                    errs() << "* CallInst Escapes:" << CI << "\n";
                    continue;
                } else {
                    errs() << "* CallInst DOES NOT Escape, removing:" << CI << "\n";
                    CI.replaceAllUsesWith(UndefValue::get(CI.getType()));
                    CI.eraseFromParent();
                }

            }

        }

    }

};

