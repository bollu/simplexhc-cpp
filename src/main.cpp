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
static Function *getOrCreateFunction(Module &m, FunctionType *FTy,
                                     std::string name) {
    Function *F = m.getFunction(name);
    if (F) return F;

    return Function::Create(FTy, GlobalValue::ExternalLinkage, name, &m);
}

static CallInst *CreateTailCall(StgIRBuilder &builder, Value *Fn,
                                ArrayRef<Value *> Args,
                                const Twine &Name = "") {
    CallInst *Call = builder.CreateCall(Fn, Args, Name);
    Call->setTailCallKind(CallInst::TCK_MustTail);
    return Call;
}

static const int stackSize = 5000;
struct BuildCtx {
   public:
    using BindingMapTy = std::map<Binding *, Function *>;

    using TypeMapTy = std::map<ConstructorName, Type *>;

    Function *printInt, *popInt, *pushInt;
    Function *malloc;
    Function *pushReturnCont, *popReturnCont;
    GlobalVariable *stackInt;
    GlobalVariable *stackIntTop;

    // type of values in return stack: () -> ()
    Type *ReturnContTy;
    // stack of return values, in reality a large array
    GlobalVariable *stackReturnCont;
    // pointer to offset to top of return stack.
    GlobalVariable *stackReturnContTop;

    BuildCtx(Module &m, StgIRBuilder &builder) {
        populateIntrinsicTypes(m, builder, typemap);

        printInt = getOrCreateFunction(m,
                                       FunctionType::get(builder.getVoidTy(),
                                                         /*isVarArg=*/false),
                                       "printInt");

        malloc = getOrCreateFunction(
            m,
            FunctionType::get(builder.getInt8Ty()->getPointerTo(),
                              {builder.getInt64Ty()}, false),
            "malloc");
        // *** Int ***
        addStack(m, builder, builder.getInt64Ty(), "Int", stackSize,
                pushInt, popInt, stackInt, stackIntTop);


        // type of returns.
        ReturnContTy =  FunctionType::get(builder.getVoidTy(), {}, /*isVarArg=*/false);
        addStack(m, builder,ReturnContTy, "Return", stackSize,
                pushReturnCont, popReturnCont, stackReturnCont, stackReturnContTop);

        // *** Return */
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

    static void addStack(Module &m, StgIRBuilder &builder, Type *elemTy,
                         std::string name, size_t size, Function *&pushFn,
                         Function *&popFn, GlobalVariable *&stack,
                         GlobalVariable *&stackTop) {
        popFn = getOrCreateFunction(
            m, FunctionType::get(builder.getInt64Ty(), /*isVarArg=*/false),
            "pop" + name);
        pushFn = getOrCreateFunction(
            m,
            FunctionType::get(builder.getVoidTy(), {builder.getInt64Ty()},
                              /*isVarArg=*/false),
            "push" + name);
        Type *stackTy = ArrayType::get(builder.getInt64Ty(), size);
        // Constant *Init = ConstantAggregateZero::get(stackTy);
        stack = new GlobalVariable(m, stackTy, /*isConstant=*/false,
                                   GlobalValue::ExternalLinkage, /*Initializer=*/nullptr,
                                   "stack" + name);

        stackTop = new GlobalVariable(
            m, builder.getInt64Ty(), /*isConstant=*/false,
            GlobalValue::ExternalLinkage,
            ConstantInt::get(builder.getInt64Ty(), 0), "stack" + name + "Top");

        addPushToModule(m, builder, pushFn, stackTop, stack);
        addPopToModule(m, builder, popFn, stackTop, stack);
    }

    static void addPushToModule(Module &m, StgIRBuilder &builder, Function *F,
                                Value *stackTop, Value *stack) {
        assert(F);
        assert(stackTop);
        assert(stack);

        BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
        builder.SetInsertPoint(entry);
        // pushInt has only one argument
        for (Argument &arg : F->args()) {
            arg.setName("i");
            Value *idx = builder.CreateLoad(stackTop, "idx");
            Value *stackSlot =
                builder.CreateGEP(stack, {builder.getInt64(0), idx}, "slot");
            builder.CreateStore(&arg, stackSlot);

            idx = builder.CreateAdd(idx, builder.getInt64(1), "idx_inc");
            builder.CreateStore(idx, stackTop);
            builder.CreateRetVoid();
        }
    }

    static void addPopToModule(Module &m, StgIRBuilder &builder, Function *F,
                               Value *stackTop, Value *stack) {
        assert(F);
        assert(stackTop);
        assert(stack);

        BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
        builder.SetInsertPoint(entry);

        Value *idx = builder.CreateLoad(stackTop, "idx");
        idx = builder.CreateSub(idx, builder.getInt64(1), "idx_dec");
        Value *stackSlot =
            builder.CreateGEP(stack, {builder.getInt64(0), idx}, "slot");
        Value *Ret = builder.CreateLoad(stackSlot, "val");

        builder.CreateStore(idx, stackTop);
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

// case over an identifier
void materializeCaseIdent(const ExpressionCase *c, Module &m,
                          StgIRBuilder &builder, BuildCtx &bctx) {
    const Identifier scrutinee = cast<AtomIdent>(c->getScrutinee())->getIdent();
    // TODO: come up with a notion of scope.
    Function *Next = bctx.getFunctionFromName(scrutinee);
    CreateTailCall(builder, Next, {});
    // push a return continuation for the function `Next` to follow.
    //bctx.pushCont(
}

void materializeCase(const ExpressionCase *c, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx) {
    if (isa<AtomInt>(c->getScrutinee())) {
        assert(false && "primitive case unimplemented");
    } else {
        materializeCaseIdent(c, m, builder, bctx);
    }
}

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
            materializeCase(cast<ExpressionCase>(e), m, builder, bctx);
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
