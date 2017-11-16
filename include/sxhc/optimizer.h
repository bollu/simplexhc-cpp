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

template<typename T>
std::set<T> intersect(std::set<T> A, std::set<T> B) {
    std::set<T> result;
    std::set_intersection(A.begin(), A.end(), B.begin(), B.end(), std::inserter(result, result.begin()));
    return result;
}

using namespace llvm;
// Pass to match abstract stack manipulations and eliminate them.
class StackMatcherPass : public PassInfoMixin<StackMatcherPass> {
public:
    explicit StackMatcherPass(StringRef stackname) : stackname(stackname) {}
    static StringRef name() { return "stackMatcher"; }

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {

        static int count = 1;
        // 95: fails
        // 85: fails
        // 70: fails
        // 60: fails
        // 55: fails
        // 53: fails
        // 52: fails
        // 51: works
        // 50: works
        // 40: works
        // 38: works
        // 35: works
        // 30: works
        // 20: works
        // 0: works
        static const int BREAK_COUNT = 100;

        if (F.isDeclaration()) { return llvm::PreservedAnalyses::all(); }

        if (count >= BREAK_COUNT) {
            return llvm::PreservedAnalyses::all();
        }

        if (count == BREAK_COUNT - 1) {
            errs() << "F before optimisation:\n" << F << "\n---\n";
        }
        DominatorTree &DT = FAM.getResult<DominatorTreeAnalysis>(F);
        assert(!F.isDeclaration() && "expected F to be a definition.");
        std::set<PushPopPair> replacements =  visitBB(F.getEntryBlock(), std::stack<CallInst *>(), DT, std::set<BasicBlock *>());



        for (PushPopPair r : replacements) {
            StackMatcherPass::propogatePushToPop(r);
        }

        // first propogate then delete because multiple pushes can use the same pop.
        // eg:
        //       A - push
        //      / \
        // pop- B  C-pop
        //
        // We expect the push to be propogated to both B and C, and then we remove the push.
        for(PushPopPair r : replacements) {
            r.first->eraseFromParent();

        }

        if (count == BREAK_COUNT - 1) {
            errs() << "F after optimisation:\n" << F << "\n---\n";
        }
        count++;
        return llvm::PreservedAnalyses::none();
    }

    void getAnalysisUsage(AnalysisUsage &AU) const {
        AU.addRequired<DominatorTreeWrapperPass>();
    }


private:
    std::string stackname;

    using PushPopPair = std::pair<CallInst *, CallInst *>;


    // return set of instructions to delete.
    std::set<PushPopPair> visitBB(BasicBlock &BB, std::stack<CallInst *> pushStack, const DominatorTree &DT, std::set<BasicBlock *> Visited) {
        Visited.insert(&BB);
        std::set<PushPopPair> replacements;

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

                // dbgs() << "popping: " << *CI << " | replacing with: " << *Push << "\n";
                replacements.insert(std::make_pair(Push, CI));

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
        bool visitedOneChild = false;

        // The set of push/pops that are safe to remove are those that are common among all children.
        std::set<PushPopPair> allPps;
        for(int i = 0; i < TI->getNumSuccessors(); i++) {
            BasicBlock *Next = TI->getSuccessor(i);
            // if (Visited.count(Next)) continue;
            //assert(false && "fixme, understand why this screws up.");

            if (!DT.dominates(&BB, Next)) continue;
            std::set<PushPopPair> pps = visitBB(*Next, pushStack, DT, Visited);
            if (visitedOneChild) {
                allPps = intersect(pps, allPps);
            }
            else {
                visitedOneChild = true;
                allPps = pps;
            }
        }

        return replacements;

    }

    static void propogatePushToPop (PushPopPair r) {
        CallInst *Push = r.first;
        CallInst *Pop = r.second;
        Value *PushedVal = Push->getArgOperand(0);
        BasicBlock::iterator ii(Pop);
        ReplaceInstWithValue(Pop->getParent()->getInstList(), ii, PushedVal);
    }


};

