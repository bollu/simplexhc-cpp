#pragma once
#include <iostream>
#include <set>
#include <sstream>
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
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "sxhc/StackAnalysis.h"
#include "sxhc/types.h"

#define DEBUG_TYPE "stackMatcher"
bool debug = false;

class PushPopMatch {
   public:
    PushPopMatch(StackInstruction push, StackInstruction pop) : pop(pop) {
        assert(pop.isPop());
        insertPush(push);
    };

    StackInstruction getPop() { return pop; }

    void insertPush(StackInstruction push) {
        assert(push.isPush());
        assert(push.getPushedVal()->getType() == pop.getType());
        pushes.push_back(push);
    }

    unsigned getNumPushes() { return pushes.size(); }

    StackInstruction getPush(unsigned n) {
        assert(n < pushes.size());
        return pushes[n];
    }

    Type *getType() {
        return pop.getType();
    }

   private:
    StackInstruction pop;
    std::vector<StackInstruction> pushes;
};

using namespace llvm;
// Pass to match abstract stack manipulations and eliminate them.
template<const char *stackName>
class StackMatcherPass : public PassInfoMixin<StackMatcherPass<stackName>> {
   public:
    explicit StackMatcherPass() {};
    static StringRef name() { return "StackMatcher"; }

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {
        if (F.isDeclaration()) {
            return llvm::PreservedAnalyses::all();
        }

       StackAnalysis &SA = FAM.getResult<StackAnalysisPass<stackName>>(F);
       std::vector<PushPopMatch> matches;

       std::vector<PushPopMatch> intraBBMatches = computeIntraBBMatches(SA, F);
       matches.insert(matches.begin(), intraBBMatches.begin(), intraBBMatches.end());

       for(PushPopMatch ppm : matches) {
           propogatePushesToPop(ppm);
       }

        {
            std::set<CallInst *> pushes;
            for (PushPopMatch ppm : matches) {
                for(unsigned i = 0; i < ppm.getNumPushes(); i++) {
                    pushes.insert(ppm.getPush(i).getInstruction());
                }
            }

            for (CallInst *push : pushes) {
                push->eraseFromParent();
            }
        }

        return llvm::PreservedAnalyses::none();
    }

    /*
    void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.addRequired<DominatorTreeWrapperPass>();
    }*/

   private:
    std::string stackname;

    static std::vector<PushPopMatch> computeIntraBBMatches(StackAnalysis &SA, Function &F) {
        std::vector<PushPopMatch> matches;
        for(BasicBlock &BB : F) {
            auto it = SA.find(&BB);
            assert(it != SA.end());
            // lookup
            StackBB SBB = it->second;
            std::stack<StackInstruction> pushes;
            for (StackInstruction SI : SBB) {
                if (SI.isPush()) {
                    pushes.push(SI);
                    continue;
                }

                assert(SI.isPop());

                // we have no push to match with, continue.
                if (pushes.size() == 0) continue;

                // grab the topmost push and use this.
                StackInstruction push = pushes.top();
                pushes.pop();

                // add the match.
                matches.push_back(PushPopMatch(push, SI));

            }

        }

        return matches;
    }

    static void propogatePushesToPop(PushPopMatch matches) {
        llvm::CallInst *PopInst = matches.getPop().getInstruction();

        // create the PHI node that replaces the pop as a PHI of all the pushes.
        errs() << "numPushes: " << matches.getNumPushes() << "\n";
        PHINode *PopReplacement =
            PHINode::Create(matches.getType(), matches.getNumPushes(),
                            PopInst->getName() + ".phi");
        for (unsigned i = 0; i < matches.getNumPushes(); i++) {
            Value *pushValue = matches.getPush(i).getPushedVal();
            BasicBlock *pushBB = matches.getPush(i).getInstruction()->getParent();
            PopReplacement->addIncoming(pushValue, pushBB);
        }

        // Replace the pop instruction with the PHI node.
        BasicBlock::iterator ii(PopInst);
        ReplaceInstWithInst(PopInst->getParent()->getInstList(), ii,
                             PopReplacement);
    }
};

Instruction *getOwningInst(User *U) {
    if (isa<Instruction>(U)) return cast<Instruction>(U);
    if (Constant *C = dyn_cast<Constant>(U)) {
        assert(C->getNumUses() == 1 && "constant has more than one use");
        return getOwningInst(*C->users().begin());
    }

    report_fatal_error("unreachable code in getOwningInst");
}

// NOTE: this pass is most likely 100% wrong in the presence of loops. Soo.. fix
// that plz :3
template<const char *stackName>
class SinkPushPass : public PassInfoMixin<SinkPushPass<stackName>> {
   public:
    SinkPushPass() {};
    static StringRef name() { return "SinkPush"; }

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {
        if (F.isDeclaration()) {
            return llvm::PreservedAnalyses::all();
        }

        if (F.getName() != "main") return llvm::PreservedAnalyses::all();

        // map from push to pops that accept it.
        std::map<CallInst *, std::vector<CallInst *>> pushToAcceptingPops =
            [&F, this] {
                std::map<CallInst *, std::vector<CallInst *>> matchedPops;
                std::set<BasicBlock *> Visited;
                BasicBlock *Entry = &F.getEntryBlock();

                findPushAndMatch(stackname, Entry, Entry->begin(), matchedPops,
                                 Visited);
                return matchedPops;
            }();

        for (auto it : pushToAcceptingPops) {
            errs() << "Push: " << *it.first;
            for (CallInst *CI : it.second) {
                errs() << "\tPop: " << *CI << "\n";
            }
        }
        return llvm::PreservedAnalyses::none();
    }

    void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.addRequired<DominatorTreeWrapperPass>();
    }

   private:
    const std::string stackname;

    static bool isInstPop(const Instruction *I, const std::string stackname) {
        const CallInst *CI = dyn_cast<CallInst>(I);
        if (!CI) return false;
        if (!CI->getCalledFunction()) return false;
        return CI->getCalledFunction()->getName() == "pop" + stackname;
    }

    static bool isInstPush(const Instruction *I, const std::string stackname) {
        const CallInst *CI = dyn_cast<CallInst>(I);
        if (!CI) return false;
        if (!CI->getCalledFunction()) return false;
        return CI->getCalledFunction()->getName() == "push" + stackname;
    }

    static void findPushAndMatch(
        const std::string stackname, BasicBlock *CurBB, BasicBlock::iterator it,
        std::map<CallInst *, std::vector<CallInst *>> &matchedPops,
        std::set<BasicBlock *> &Visited) {
        assert(!Visited.count(CurBB));
        for (; it != CurBB->end(); ++it) {
            Instruction *I = &*it;
            if (!isInstPush(I, stackname)) continue;
            CallInst *CI = cast<CallInst>(I);
            // we fond a push
            getMatchingPops(stackname.c_str(), CurBB, it, CI, matchedPops, Visited);
            return;
        }
        // no pushes found
        Visited.insert(CurBB);

        // BB had no pops.
        const Instruction *TI = CurBB->getTerminator();
        assert(TI && "BB has no terminator!");
        for (unsigned i = 0; i < TI->getNumSuccessors(); i++) {
            BasicBlock *Next = TI->getSuccessor(i);
            if (Next->getUniquePredecessor() == CurBB) {
                findPushAndMatch(stackname, Next, Next->begin(), matchedPops,
                                 Visited);
            }
        }
    }

    static void getMatchingPops(
        const char *stackname, BasicBlock *CurBB, BasicBlock::iterator it,
        CallInst *push,
        std::map<CallInst *, std::vector<CallInst *>> &matchedPops,
        std::set<BasicBlock *> &Visited) {
        assert(!Visited.count(CurBB));
        if (Visited.count(CurBB)) return;

        for (; it != CurBB->end(); ++it) {
            Instruction *I = &*it;
            if (!isInstPop(I, stackname)) continue;
            // we found a match.  So now, switch to push-hunting mode.
            matchedPops[push].push_back(cast<CallInst>(I));
            findPushAndMatch(stackname, CurBB, it, matchedPops, Visited);
            return;
        }

        // we finished the iterator, add us to `visited`.
        Visited.insert(CurBB);

        // BB had no pops.
        const Instruction *TI = CurBB->getTerminator();
        assert(TI && "BB has no terminator!");
        for (unsigned i = 0; i < TI->getNumSuccessors(); i++) {
            BasicBlock *Next = TI->getSuccessor(i);
            if (Next->getUniquePredecessor() == CurBB) {
                getMatchingPops(stackname, Next, Next->begin(), push,
                                matchedPops, Visited);
            }
        }
    }
};

class EliminateUnusedAllocPass
    : public PassInfoMixin<EliminateUnusedAllocPass> {
   public:
    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {
        if (F.isDeclaration()) {
            return llvm::PreservedAnalyses::all();
        }
        // hack.
        if (true) {
            return llvm::PreservedAnalyses::all();
        }
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
    static bool isUserThatCreatesEscape(Instruction *I,
                                        Value *MaybeEscapingVal) {
        if (auto CI = dyn_cast<CallInst>(I)) {
            if (!CI->getCalledFunction()) {
                report_fatal_error(
                    "indirect function call allows pointer to escape.");
            }
            if (CI->getName() == "pushReturn") return true;
            return false;
        } else if (auto SI = dyn_cast<StoreInst>(I)) {
            // store  <val> <maybeescapingval> does NOT escape.
            // store  <maybeescapingval> <escapeloc> DOES escape.
            return SI->getOperand(0) == MaybeEscapingVal;
        } else if (isa<LoadInst>(I)) {
            return false;
        } else if (isa<BitCastInst>(I) || isa<GetElementPtrInst>(I)) {
            return doesEscape(I, MaybeEscapingVal);
        }
        errs() << "unknown instruction pass to " << __PRETTY_FUNCTION__ << ": "
               << *I << "\n";
        report_fatal_error("unknown instruction.");
    }

    static bool doesEscape(Instruction *I, Value *MaybeEscapingVal) {
        for (User *U : I->users()) {
            Instruction *User = getOwningInst(U);
            if (isUserThatCreatesEscape(User, MaybeEscapingVal)) return true;
        }
        return false;
    }

    void runInternal(Function &F, AAResults &AA) {
        errs() << "Eliminate unused alloc running on: " << F.getName() << "\n";

        for (BasicBlock &BB : F) {
            for (Instruction &I : BB) {
                if (!isa<CallInst>(I)) continue;
                CallInst &CI = cast<CallInst>(I);
                // we don't analyse indirect calls.
                if (!CI.getCalledFunction()) continue;

                assert(CI.getCalledFunction() &&
                       "should have bailed out by this point at an indirect "
                       "function");
                if (CI.getCalledFunction()->getName() != "alloc") {
                    errs() << "* CallInst Escapes:" << CI << "\n";
                    continue;
                } else {
                    errs() << "* CallInst DOES NOT Escape, removing:" << CI
                           << "\n";
                    CI.replaceAllUsesWith(UndefValue::get(CI.getType()));
                    CI.eraseFromParent();
                }
            }
        }
    }
};
