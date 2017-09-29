#include <iostream>
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "stgir.h"

using namespace std;
using namespace stg;
using namespace llvm;

using StgIRBuilder = IRBuilder<>;

// http://web.iitd.ac.in/~sumeet/flex__bison.pdf
// http://aquamentus.com/flex_bison.html
//
//

// Get a function with name `name`, and if it doesn't exist, create
// a function with name `name`, type `FTy`, module `m`.
Function *getOrCreateFunction(Module &m, FunctionType *FTy, std::string name) {
    Function *F = m.getFunction(name);
    if (F) return F;

    return Function::Create(FTy, GlobalValue::ExternalLinkage, name, &m);
}

static const int stackSize = 5000;
struct BuildCtx {
   private:
    void buildPushInt(Module &m, StgIRBuilder &builder) {
        assert(pushInt);
        assert(stackPrimTop);
        assert(stackPrim);

        BasicBlock *entry =
            BasicBlock::Create(m.getContext(), "entry", pushInt);
        builder.SetInsertPoint(entry);
        for (Argument &arg : pushInt->args()) {
            arg.setName("i");
            Value *idx = builder.CreateLoad(stackPrimTop, "idx");
        Value *stackSlot = builder.CreateGEP(stackPrim, {builder.getInt64(0), idx}, "slot");
            builder.CreateStore(&arg, stackSlot);

            idx = builder.CreateAdd(idx, builder.getInt64(1), "idx_inc");
            builder.CreateStore(idx, stackPrimTop);
            builder.CreateRetVoid();

        }
    }

    void buildPopInt(Module &m, StgIRBuilder &builder) {
        assert(popInt);
        assert(stackPrimTop);
        assert(stackPrim);

        BasicBlock *entry =
            BasicBlock::Create(m.getContext(), "entry", popInt);
        builder.SetInsertPoint(entry);


        Value *idx = builder.CreateLoad(stackPrimTop, "idx");
        idx = builder.CreateSub(idx, builder.getInt64(1), "idx_dec");
        Value *stackSlot = builder.CreateGEP(stackPrim, {builder.getInt64(0), idx}, "slot");
        Value *Ret = builder.CreateLoad(stackSlot, "val");

        builder.CreateStore(idx, stackPrimTop);
        builder.CreateRet(Ret);

    }

   public:
    Function *printInt, *popInt, *pushInt;
    GlobalVariable *stackPrim;
    GlobalVariable *stackPrimTop;
    using BindingMapTy = std::map<Binding *, Function *>;
    BindingMapTy bindingmap;

    BuildCtx(Module &m, StgIRBuilder &builder) {
        printInt = getOrCreateFunction(m,
                                       FunctionType::get(builder.getVoidTy(),
                                                         /*isVarArg=*/false),
                                       "printInt");
        // printInt = getOrCreateFunction(
        //     m,
        //     FunctionType::get(builder.getVoidTy(), {builder.getInt64Ty()},
        //                      /*isVarArg=*/false),
        //    "printInt");

        popInt = getOrCreateFunction(
            m, FunctionType::get(builder.getInt64Ty(), /*isVarArg=*/false),
            "popInt");
        pushInt = getOrCreateFunction(
            m,
            FunctionType::get(builder.getVoidTy(), {builder.getInt64Ty()},
                              /*isVarArg=*/false),
            "pushInt");
        Type *stackPrimTy = ArrayType::get(builder.getInt64Ty(), stackSize);
        Constant *stackPrimInit = ConstantAggregateZero::get(stackPrimTy);
        stackPrim = new GlobalVariable(m, stackPrimTy, /*isConstant=*/false,
                                       GlobalValue::ExternalLinkage,
                                       stackPrimInit, "stackPrim");

        stackPrimTop = new GlobalVariable(
            m, builder.getInt64Ty(), /*isConstant=*/false,
            GlobalValue::ExternalLinkage,
            ConstantInt::get(builder.getInt64Ty(), 0), "stackPrimTop");
        buildPushInt(m, builder);
        buildPopInt(m, builder);
    }

    Function *getFunctionFromName(std::string name) {
        if (name == "printInt") return printInt;
        for (auto It : bindingmap)
            if (It.first->getName() == name) return It.second;
        cerr << __PRETTY_FUNCTION__ << " |unknown function with name: " << name
             << "\n";
        assert(false && "unknown function");
    }
};

Value *materializeAtom(const AtomInt *i, StgIRBuilder &builder) {
    return builder.getInt64(i->getVal());
}

void materializeExpr(const ExpressionAp *ap, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx) {
    for (Atom *p : *ap) {
        AtomInt *i = cast<AtomInt>(p);
        Value *v = materializeAtom(i, builder);
        builder.CreateCall(bctx.pushInt, {v});

        Function *Cont = bctx.getFunctionFromName(ap->getFnName());
        builder.CreateCall(Cont);
    }
};

void materializeExpr(const Expression *e, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx) {
    cout << e->getKind() << "|" << *e << "\n";
    switch (e->getKind()) {
        case Expression::EK_Ap:
            materializeExpr(cast<ExpressionAp>(e), m, builder, bctx);
            break;
        case Expression::EK_Cons:            
        case Expression::EK_Case:
            assert(false && "unimplemented");
            break;
    };
}

void materializeLambda(const Lambda *l, Module &m, StgIRBuilder &builder,
                       BuildCtx &bctx) {
    for(const Parameter *p : *l) {
        cout << "parameter: " << *p;
    }
    materializeExpr(l->getRhs(), m, builder, bctx);
}

Function *materializeBinding(const Binding *b, Module &m, StgIRBuilder &builder,
                             BuildCtx &bctx) {
    FunctionType *FTy =
        FunctionType::get(builder.getVoidTy(), /*isVarArg=*/false);
    Function *F = Function::Create(FTy, GlobalValue::ExternalLinkage,
                                   b->getName(), &m);

    BasicBlock *Entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(Entry);
    materializeLambda(b->getRhs(), m, builder, bctx);
    builder.CreateRetVoid();
    return F;
}



void compile_program(stg::Program *program) {
    cout << "> program: " << *program << "\n";
    LLVMContext ctx;
    StgIRBuilder builder(ctx);
    Module m("Module", ctx);

    BuildCtx bctx(m, builder);

    Binding *entrystg = nullptr;

    for (Binding *b : program->bindings_range()) {
        if (b->getName() == "main") {
            assert(!entrystg && "program has more than one main.");
            entrystg = b;
        }
        bctx.bindingmap[b] = materializeBinding(b, m, builder, bctx);
    }
    m.print(errs(), nullptr);
}
