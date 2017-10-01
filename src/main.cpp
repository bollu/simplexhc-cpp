#include <iostream>
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
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
   public:
    using BindingMapTy = std::map<Binding *, Function *>;

    using TypeMapTy = std::map<ConstructorName, Type *>;

    Function *printInt, *popInt, *pushInt, *malloc;
    GlobalVariable *stackPrim;
    GlobalVariable *stackPrimTop;

    BuildCtx(Module &m, StgIRBuilder &builder) {
        populateIntrinsicTypes(m, builder, typemap);

        printInt = getOrCreateFunction(m,
                                       FunctionType::get(builder.getVoidTy(),
                                                         /*isVarArg=*/false),
                                       "printInt");

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

        malloc = getOrCreateFunction(
            m,
            FunctionType::get(builder.getInt8Ty()->getPointerTo(),
                              {builder.getInt64Ty()}, false),
            "malloc");

        stackPrimTop = new GlobalVariable(
            m, builder.getInt64Ty(), /*isConstant=*/false,
            GlobalValue::ExternalLinkage,
            ConstantInt::get(builder.getInt64Ty(), 0), "stackPrimTop");
        addPushIntToModule(m, builder);
        addPopIntToModule(m, builder);
    }

    void insertBinding(Binding *b, Function *f) { bindingmap[b] = f; }

    Function *getFunctionFromName(std::string name) const {
        if (name == "printInt") return printInt;
        for (auto It : bindingmap)
            if (It.first->getName() == name) return It.second;

        cerr << __PRETTY_FUNCTION__ << " |unknown function with name: " << name
             << "\n";
        assert(false && "unknown function");
    }

    void insertType(std::string name, Type *T) { typemap[name] = T; }

    Type *getTypeFromName(std::string name) const {
        auto It = typemap.find(name);
        if (It == typemap.end()) {
            cerr << __PRETTY_FUNCTION__ << " |unknown type with name: " << name
                 << "\n";
            errs() << "TypeMap:\n";
            for (auto It : typemap) {
                errs() << It.first << " -> " << *It.second << "\n";
            }
            errs() << "---\n";
            assert(false && "unknown type name");
        }
        return It->second;
    }

   private:
    BindingMapTy bindingmap;
    TypeMapTy typemap;

    static void populateIntrinsicTypes(Module &m, StgIRBuilder &builder,
                                       TypeMapTy &map) {
        map["PrimInt"] = builder.getInt64Ty();
    }

    void addPushIntToModule(Module &m, StgIRBuilder &builder) {
        assert(pushInt);
        assert(stackPrimTop);
        assert(stackPrim);

        BasicBlock *entry =
            BasicBlock::Create(m.getContext(), "entry", pushInt);
        builder.SetInsertPoint(entry);
        for (Argument &arg : pushInt->args()) {
            arg.setName("i");
            Value *idx = builder.CreateLoad(stackPrimTop, "idx");
            Value *stackSlot = builder.CreateGEP(
                stackPrim, {builder.getInt64(0), idx}, "slot");
            builder.CreateStore(&arg, stackSlot);

            idx = builder.CreateAdd(idx, builder.getInt64(1), "idx_inc");
            builder.CreateStore(idx, stackPrimTop);
            builder.CreateRetVoid();
        }
    }

    void addPopIntToModule(Module &m, StgIRBuilder &builder) {
        assert(popInt);
        assert(stackPrimTop);
        assert(stackPrim);

        BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", popInt);
        builder.SetInsertPoint(entry);

        Value *idx = builder.CreateLoad(stackPrimTop, "idx");
        idx = builder.CreateSub(idx, builder.getInt64(1), "idx_dec");
        Value *stackSlot =
            builder.CreateGEP(stackPrim, {builder.getInt64(0), idx}, "slot");
        Value *Ret = builder.CreateLoad(stackSlot, "val");

        builder.CreateStore(idx, stackPrimTop);
        builder.CreateRet(Ret);
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

void materializeExpr(const ExpressionConstructor *c, Module &m,
                     StgIRBuilder &builder, BuildCtx &bctx) {
    int TotalSize = 0;
    for (Atom *a : c->args_range()) {
        AtomInt *ai = cast<AtomInt>(a);

        TotalSize += 4;  // bytes.
    }
    Value *rawMem = builder.CreateCall(bctx.malloc,
                                       {builder.getInt64(TotalSize)}, "rawmem");
    Type *structType = bctx.getTypeFromName(c->getName());
    Value *typedMem =
        builder.CreateBitCast(rawMem, structType->getPointerTo(), "typedmem");
    // Push values into the constructed value
    unsigned i = 0;
    for (Atom *a : c->args_range()) {
        AtomInt *ai = cast<AtomInt>(a);
        std::vector<Value *> idxs = {builder.getInt64(0), builder.getInt32(0)};
        Value *indexedMem = builder.CreateGEP(
            typedMem, idxs, "indexedmem_" + std::to_string(i));

        Value *v = materializeAtom(ai, builder);
        v->setName("param_" + std::to_string(i));
        builder.CreateStore(v, indexedMem);
        i++;
    }
    Value *memAddr =
        builder.CreatePtrToInt(typedMem, builder.getInt64Ty(), "memaddr");
    builder.CreateCall(bctx.pushInt, {memAddr});
};

void materializeExpr(const Expression *e, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx) {
    switch (e->getKind()) {
        case Expression::EK_Ap:
            materializeExpr(cast<ExpressionAp>(e), m, builder, bctx);
            break;
        case Expression::EK_Cons:
            materializeExpr(cast<ExpressionConstructor>(e), m, builder, bctx);
            break;
        case Expression::EK_Case:
            assert(false && "unimplemented");
            break;
    };
}

void materializeLambda(const Lambda *l, Module &m, StgIRBuilder &builder,
                       BuildCtx &bctx) {
    for (const Parameter *p : *l) {
        cout << "parameter: " << *p;
    }
    materializeExpr(l->getRhs(), m, builder, bctx);
}

Function *materializeBinding(const Binding *b, Module &m, StgIRBuilder &builder,
                             BuildCtx &bctx) {
    FunctionType *FTy =
        FunctionType::get(builder.getVoidTy(), /*isVarArg=*/false);
    Function *F =
        Function::Create(FTy, GlobalValue::ExternalLinkage, b->getName(), &m);

    BasicBlock *Entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(Entry);
    materializeLambda(b->getRhs(), m, builder, bctx);
    builder.CreateRetVoid();
    return F;
}

StructType *materializeDataDeclaration(const DataDeclaration *decl,
                                       const Module &m, const BuildCtx &bctx) {
    std::vector<Type *> Elements;
    for (TypeName *Name : decl->types_range()) {
        Elements.push_back(bctx.getTypeFromName(*Name));
    }

    StructType *Ty =
        StructType::create(m.getContext(), Elements, decl->getName());
    return Ty;
};

int compile_program(stg::Program *program) {
    cout << "> program: " << *program << "\n";
    LLVMContext ctx;
    StgIRBuilder builder(ctx);
    Module m("Module", ctx);

    BuildCtx bctx(m, builder);

    Binding *entrystg = nullptr;
    for (DataDeclaration *decl : program->declarations_range()) {
        bctx.insertType(decl->getName(),
                        materializeDataDeclaration(decl, m, bctx));
    }

    for (Binding *b : program->bindings_range()) {
        if (b->getName() == "main") {
            assert(!entrystg && "program has more than one main.");
            entrystg = b;
        }

        bctx.insertBinding(b, materializeBinding(b, m, builder, bctx));
    }
    m.print(errs(), nullptr);
    return 0;
}
