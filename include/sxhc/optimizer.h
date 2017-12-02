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
#include "sxhc/types.h"

#define DEBUG_TYPE "stackMatcher"
bool debug = false;
using PushPopPair = std::pair<llvm::CallInst *, llvm::CallInst *>;

// We assume that both A and B have only inter-block push and pop pairs.
std::set<PushPopPair> getCommonAllowedPushPopPairs(std::set<PushPopPair> A,
                                                   std::set<PushPopPair> B) {
    const std::set<llvm::CallInst *> commonPushes = [&] {
        std::set<llvm::CallInst *> apushes;

        for (PushPopPair pa : A) apushes.insert(pa.first);

        std::set<llvm::CallInst *> commonPushes;
        for (PushPopPair pb : B) {
            assert(pb.first != pb.second);
            if (apushes.count(pb.first)) {
                commonPushes.insert(pb.first);
            }
        }
        return commonPushes;
    }();

    std::set<PushPopPair> result;

    auto insertCommonPairs = [&](const std::set<PushPopPair> &pairs,
                                 std::set<PushPopPair> &container) {
        std::set<PushPopPair> ps;
        for (PushPopPair p : pairs)
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
    static StringRef name() { return "StackMatcher"; }

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {
        if (F.isDeclaration()) {
            return llvm::PreservedAnalyses::all();
        }
        if (F.getName() == "main" && stackname == "Int") {
            static int count = 0;
            count++;
            if (count == 100) {
                debug = true;
                errs() << __PRETTY_FUNCTION__ << "::main count: " << count
                       << "\n";
            }
        }
        // if (F.getName() != "main") return llvm::PreservedAnalyses::all();

        DominatorTree &DT = FAM.getResult<DominatorTreeAnalysis>(F);
        assert(!F.isDeclaration() && "expected F to be a definition.");
        std::set<PushPopPair> inter, intra;

        {
            std::set<BasicBlock *> Visited;
            std::tie(inter, intra) = visitBB(
                F.getEntryBlock(), std::stack<CallInst *>(), DT, Visited);
        }

        std::set<PushPopPair> replacements;
        replacements.insert(inter.begin(), inter.end());
        replacements.insert(intra.begin(), intra.end());

        for (PushPopPair r : replacements) {
            StackMatcherPass::propogatePushToPop(r, DT);
        }

        /* first propogate then delete because multiple pushes can use the same
         pop. eg: A - push
              / \
         pop- B  C-pop

         We expect the push to be propogated to both B and C, and then we remove
         the push.
         */
        {
            std::set<CallInst *> pushes;
            for (PushPopPair r : replacements) {
                pushes.insert(r.first);
            }
            for (CallInst *push : pushes) {
                push->eraseFromParent();
            }
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


    // Create a pairing for instruction "me"
    // myStack contains stack of my type
    // pairStack contains stack of dual, to be paried with
    // pairer is a function that creates a pushPopPair
    // intra and inter and the respective pair sets to be pushed into
    // if a pair is created.
    static void createPairing(
        CallInst *me, std::stack<CallInst *> &myStack,
        std::stack<CallInst *> &pairStack,
        std::function<PushPopPair(CallInst *me, CallInst *pair_)> pairer,
        IntraBlockPushPopPairs &intra, InterBlockPushPopPairs &inter) {

        // both stacks are not allowed to have elements at the same time.
        // If that was the case, they should have been paired up.
        assert(!(myStack.size() > 0 && pairStack.size() > 0));
        if (pairStack.size()) {
            CallInst *partner = pairStack.top();
            pairStack.pop();

            PushPopPair p = pairer(me, partner);
            if (me->getParent() == partner->getParent()) {
                intra.insert(p);
            } else {
                inter.insert(p);
            }
        } else {
            myStack.push(me);
        }
    }

    // intra = any set of push/pop that is below the current BB
    // inter = any push/pop that is mix of things below and above the
    // current BB.
    std::pair<IntraBlockPushPopPairs, InterBlockPushPopPairs> visitBB(
        BasicBlock &BB, std::stack<CallInst *> pushStack,
        const DominatorTree &DT,
        std::set<BasicBlock *> &Visited) {
        IntraBlockPushPopPairs intraBlockPushPopPairs;
        InterBlockPushPopPairs interBlockPushPopPairs;
        if (Visited.count(&BB))
            return std::make_pair(intraBlockPushPopPairs,
                                  interBlockPushPopPairs);

        Visited.insert(&BB);

        std::set<CallInst *> toDelete;

        for (Instruction &I : BB) {
            // We make _heavy_ assumptions about our IR: that is, that any
            // instruction we don't understand can't hurt us in particular, that
            // nothing can interfere with push/pop.
            if (!isa<CallInst>(I)) continue;

            CallInst *CI = cast<CallInst>(&I);
            // indirect call. Do not try to analyze
            if (!CI->getCalledFunction()) continue;

            const std::string calleeName = CI->getCalledFunction()->getName();
            if (calleeName == "push" + stackname) {
                pushStack.push(CI);
                // NOTE: this will wind up associating _pops_ with _pushes_ as well. Example:
                // x = pop
                // y = push(10)
                // x will be associated with y as a pushPopPair. This is _not_ what we want.
                // We wish to only go in the "forward" direction.

                // createPairing(CI, pushStack, popStack, [](CallInst *push, CallInst *pop) {
                //         return std::make_pair(push, pop);
                //         }, intraBlockPushPopPairs, interBlockPushPopPairs);

            } else if (calleeName == "pop" + stackname) {
                // NOTE: this would be  super useful as an _analysis_ of the function.
                // createPairing(CI, popStack, pushStack, [](CallInst *pop, CallInst *push) {
                //        return std::make_pair(push, pop);
                //        }, intraBlockPushPopPairs, interBlockPushPopPairs);
                // We have an unmatched pop. Continue, we can't do anything about it.
                if (!pushStack.size()) continue;
                assert(pushStack.size() >= 0);
                CallInst *Push = pushStack.top();
                pushStack.pop();
                assert(DT.dominates(Push, CI));
                PushPopPair ppp  = std::make_pair(Push, CI);
                if (Push->getParent() == CI->getParent()) {
                    intraBlockPushPopPairs.insert(ppp);
                }
                else {
                    interBlockPushPopPairs.insert(ppp);
                }
            }
        }

        // If you are next in the CFG and are dominated in the DT, then you
        // _will_ have the stack state your parent has. We need both to be
        // satisfied. (Why?) Consider a CFG:
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

        for (unsigned i = 0; i < TI->getNumSuccessors(); i++) {
            BasicBlock *Next = TI->getSuccessor(i);
            bool canForwardState = true;
            if (!DT.dominates(&BB, Next)) { canForwardState = false; };
            // We need to ensure that the only control flow into this BB must be us.
            // This will disallow cases like this:
            // Entry
            //  |
            //  v
            //  A <---*
            //  |     |
            //  |     |
            //  v    |
            //  B----*
            // Here, Entry `dom` A, but we cannot use Entry's state _into_ A.
            if (Next->getUniquePredecessor() != &BB) { canForwardState = false; }

            IntraBlockPushPopPairs curIntra;
            InterBlockPushPopPairs curInter;
            std::stack<CallInst *>childPushStack = canForwardState ? pushStack : std::stack<CallInst *>();
            std::tie(curIntra, curInter) =
                visitBB(*Next, childPushStack, DT, Visited);
            // whatever is intra to the child will definitely be intra to the
            // parent.
            intraBlockPushPopPairs.insert(curIntra.begin(), curIntra.end());

            // if we can forward our state to the child, then we can trust the inter-BB decisions our child makes.
            if (canForwardState)
                childPpsList.push_back(curInter);
        }

        // count the number of times a push is matched.
        std::map<CallInst *, unsigned> pushScoreboard;
        for (InterBlockPushPopPairs pps : childPpsList) {
            for (PushPopPair pp : pps) pushScoreboard[pp.first]++;
        }

        for (InterBlockPushPopPairs pps : childPpsList) {
            for (PushPopPair pp : pps) {
                assert(pushScoreboard.find(pp.first) != pushScoreboard.end());
                // all children access this push, then add this to an intra-block pair.
                if (pushScoreboard[pp.first] == TI->getNumSuccessors()) {
                    if (pp.first->getParent() == &BB) {
                        intraBlockPushPopPairs.insert(pp);
                    } else {
                        interBlockPushPopPairs.insert(pp);
                    }
                }
            }
        }

        return std::make_pair(intraBlockPushPopPairs, interBlockPushPopPairs);
    }

    static void propogatePushToPop(PushPopPair r, DominatorTree &DT) {
        CallInst *Push = r.first;
        CallInst *Pop = r.second;
        // errs() << "---\n";
        // errs() << "propogating push: \n\t" << *Push << "|" << Push->getParent()->getName() << "\n\t" << *Pop << "|" << Pop->getParent()->getName() << "\n";
        Value *PushedVal = Push->getArgOperand(0);

        // if (Push->getParent() == Pop->getParent()) {
        //     errs() << "Intra BB push/pop:\n";
        //     Push->getParent()->dump();
        // }
        // else {
        //     errs() << "*** INTER BB PUSH/POP***\n";
        //     Push->getParent()->getParent()->dump();
        // }
        assert(DT.dominates(Push, Pop) && "push must dominate pop to propagate them.");
        // errs() << "---\n";

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

    report_fatal_error("unreachable code in getOwningInst");
}

// NOTE: this pass is most likely 100% wrong in the presence of loops. Soo.. fix
// that plz :3
class SinkPushPass : public PassInfoMixin<SinkPushPass> {
   public:
    SinkPushPass(std::string stackname) : stackname(stackname) {}
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
            getMatchingPops(stackname, CurBB, it, CI, matchedPops, Visited);
            return;
        }
        // no pushes found
        Visited.insert(CurBB);

        // BB had no pops.
        const TerminatorInst *TI = CurBB->getTerminator();
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
        const std::string stackname, BasicBlock *CurBB, BasicBlock::iterator it,
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
        const TerminatorInst *TI = CurBB->getTerminator();
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
