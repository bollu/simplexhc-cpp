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

using namespace llvm;
// Pass to match abstract stack manipulations and eliminate them.
class StackMatcherPass : public PassInfoMixin<StackMatcherPass> {
public:
    explicit StackMatcherPass(StringRef stackname) : stackname(stackname) {}
    static StringRef name() { return "stackMatcher"; }

    PreservedAnalyses run(Function &F, FunctionAnalysisManager &FAM) {
        if (F.isDeclaration()) return llvm::PreservedAnalyses::all();

        DominatorTree &DT = FAM.getResult<DominatorTreeAnalysis>(F);
        errs() << "\nStackMatcherPass running on: |" << F.getName() << "|\n";

        assert(!F.isDeclaration() && "expected F to be a definition.");
        visitBB(F.getEntryBlock(), std::stack<Value *>(), DT);
        return llvm::PreservedAnalyses::none();
    }

private:
    std::string stackname;

    using InstReplacement = std::pair<Instruction *, Value *>;

    void visitBB(BasicBlock &BB, std::stack<Value *> matchedStack, const DominatorTree &DT) {
        std::vector<InstReplacement> replacements;

        for(Instruction &I : BB) {
            // We make _heavy_ assumptions about our IR: that is, that any instruction we don't understand can't hurt us
            // in particular, that nothing can interfere with push/pop.
            if (!isa<CallInst>(I)) continue;

            CallInst *CI = cast<CallInst>(&I);
            // indirect call. Do not try to analyze
            if (!CI->getCalledFunction()) continue;

            const std::string calleeName = CI->getCalledFunction()->getName();
            if (calleeName == "push" + stackname) {
                matchedStack.push(CI->getArgOperand(0));
                errs() << "pushing: " << *CI << "\n";

            }
            else if (calleeName == "pop" + stackname) {
                // We do not have a matching push, our function is incomplete. continue
                if (matchedStack.size() == 0) continue;
                Value *V = matchedStack.top();
                matchedStack.pop();

                errs() << "popping: " << *CI << " | replacing with: " << *V << "\n";
                replacements.push_back(std::make_pair(CI, V));

                //errs() << __LINE__ << "\n";
                //ReplaceInstWithValue(CI->getParent()->getInstList(), ii, V);
                //errs() << __LINE__ << "\n";

            }
        }

        for (InstReplacement r : replacements) {
            Instruction *Old = r.first;
            Value *New = r.second;
            BasicBlock::iterator ii(Old);
            ReplaceInstWithValue(Old->getParent()->getInstList(), ii, New);

        }
    }

};

