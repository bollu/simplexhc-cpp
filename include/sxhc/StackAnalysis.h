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
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "sxhc/types.h"

class StackInstruction {
   public:
    enum StackInstructionType { SIP_Push, SIP_Pop };


    // llvm::CallInst *getCall() const { return CI; }

    bool isPush() const { return type == SIP_Push; }
    bool isPop() const { return type == SIP_Pop; }

    llvm::StringRef getName() const {
        return CI->getName();
    }

    llvm::Value *getPushedVal() {
        assert(isPush());
        return CI->getArgOperand(0);
    }

    llvm::Type *getType() {
        return CI->getType();
    }

    llvm::CallInst *getInstruction() { return CI; }

    static StackInstruction createPush(llvm::CallInst *CI) {
        return StackInstruction(SIP_Push, CI);
    }

    static StackInstruction createPop(llvm::CallInst *CI) {
        return StackInstruction(SIP_Pop, CI);
    }

   private:
    StackInstructionType type;
    // StackInstructionType getType() const { return type; }
    llvm::CallInst *CI;

    StackInstruction(StackInstructionType type, llvm::CallInst *CI)
        : type(type), CI(CI){};
};

using StackBB = std::vector<StackInstruction>;
using StackAnalysis = std::map<llvm::BasicBlock *, StackBB>;

using namespace llvm;
template<const char *stackName>
// class StackAnalysisPass : public llvm::AnalysisInfoMixin<StackAnalysisPass<stackName>> {
class StackAnalysisPass : public llvm::PassInfoMixin<StackAnalysisPass<stackName>> {
   public:
    static AnalysisKey *ID();
    static llvm::AnalysisKey Key;

    using Result = StackAnalysis;
    StackAnalysisPass() {};
    static llvm::StringRef name(){ return "StackAnalysis"; };


    StackAnalysis run(llvm::Function &F, llvm::FunctionAnalysisManager &FAM) {
        StackAnalysis SA;
        for (llvm::BasicBlock &BB : F) {
            SA[&BB] = makeAnalaysis(&BB);
        }
        return SA;
    };

   private:
    friend llvm::AnalysisInfoMixin<StackAnalysisPass<stackName>>;


    StackBB makeAnalaysis(llvm::BasicBlock *BB) {
        StackBB stackbb;

        for (llvm::Instruction &I : *BB) {
            llvm::CallInst *CI = dyn_cast<llvm::CallInst>(&I);

            if (CI == nullptr) continue;
            if (!CI->getCalledFunction()) continue;

            const std::string funcname = CI->getCalledFunction()->getName();

            if (funcname == std::string("push") + std::string(stackName)) {
                stackbb.push_back(StackInstruction::createPush(CI));
            } else if (funcname == std::string("pop") + std::string(stackName)) {
                stackbb.push_back(StackInstruction::createPop(CI));
            }
        }
        return stackbb;
    }
};

