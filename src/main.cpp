#include <iostream>
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
#include "llvm/Support/FileSystem.h"
#include "stgir.h"

using namespace std;
using namespace stg;
using namespace llvm;

class BuildCtx;

using StgIRBuilder = IRBuilder<>;

void materializeExpr(const Expression *e, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx);
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

template <typename K, typename V>
class Scope {
   public:
    using MapTy = std::map<K, V>;
    using iterator = typename MapTy::iterator;
    using const_iterator = typename MapTy::const_iterator;

   private:
    MapTy m;
    Optional<Scope<K, V> *> inner;

   public:
    Scope() : inner(None) {};

    void insert(K k, V v) {
        if (inner)
            inner.getValue()->insert(k, v);
        else {
            assert(m.find(k) == m.end());
            m[k] = v;
        }
    }

    iterator end() { return m.end(); }
    const_iterator end() const { return m.end(); }


    // If our inner scope has this value, give it. Otherwise, default
    // and give what we have.
    // TODO: find a way to reduce code duplication b/w the two find.
    iterator find(K k) {
        if (inner) {
            auto It = inner.getValue()->find(k);
            if (It != inner.getValue()->end())
                return It;
        }

        return m.find(k);
    }

    const_iterator find(K k) const {
        if (inner) {
            auto It = inner.getValue()->find(k);
            if (It != inner.getValue()->end())
                return It;
        }

        return m.find(k);
    }


    void pushScope() {
        if (!inner) {
            inner = Optional<Scope<K, V>>(new Scope());
        } else {
            inner->pushScope();
        }
    }

    void popScope() {
        assert(inner && "calling popScope on the innermost scope");
        if (inner->isInnermostScope()) {
            inner = None;
        }
        else {
            inner->popScope();
        }
    }

    bool isInnermostScope() const {
        return !inner;
    }
};

static const int stackSize = 5000;
struct BuildCtx {
   public:
    using IdentifierMapTy = Scope<Identifier, Value *>;

    using TypeMapTy =
        std::map<ConstructorName, std::pair<DataDeclaration *, Type *>>;

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
        identifiermap.insert("printInt", printInt);

        malloc = getOrCreateFunction(
            m,
            FunctionType::get(builder.getInt8Ty()->getPointerTo(),
                              {builder.getInt64Ty()}, false),
            "malloc");
        // *** Int ***
        addStack(m, builder, builder.getInt64Ty(), "Int", stackSize, pushInt,
                 popInt, stackInt, stackIntTop);

        // type of returns.
        ReturnContTy =
            FunctionType::get(builder.getVoidTy(), {}, /*isVarArg=*/false)
                ->getPointerTo();
        addStack(m, builder, ReturnContTy, "Return", stackSize, pushReturnCont,
                 popReturnCont, stackReturnCont, stackReturnContTop);

        // *** Return */
    }

    void insertBinding(Binding *b, Function *f) { 
        identifiermap.insert(b->getName(), f);
    }

    Function *getFunctionFromName(std::string name) const {
        errs() << __PRETTY_FUNCTION__<< "name: " << name << "\n";
        auto It = identifiermap.find(name);
        if (It == identifiermap.end()) {
            assert(false && "function not found");
        }
        Value *V = It->second;
        if (!isa<Function>(V)) {
            assert(false && "expected function, found value");
        }
        return cast<Function>(V);
    }

    void insertType(std::string name, DataDeclaration *decl, Type *type) {
        typemap[name] = std::make_pair(decl, type);
    }

    std::pair<DataDeclaration *, Type *> getTypeFromName(
        std::string name) const {
        auto It = typemap.find(name);
        if (It == typemap.end()) {
            cerr << __PRETTY_FUNCTION__ << " |unknown type with name: " << name
                 << "\n";
            errs() << "TypeMap:\n";
            // TODO: teach my types to use LLVM streams.
            for (auto It : typemap) {
                errs() << It.first << " -> ";
                if (It.second.first)
                    cerr << *(It.second.first);
                else
                    errs() << "nullptr";
                errs() << ", " << *(It.second.second) << "\n";
            }
            errs() << "---\n";
            assert(false && "unknown type name");
        }
        return It->second;
    }

   private:
    IdentifierMapTy identifiermap;
    TypeMapTy typemap;

    static void populateIntrinsicTypes(Module &m, StgIRBuilder &builder,
                                       TypeMapTy &map) {
        map["PrimInt"] = std::make_pair(nullptr, builder.getInt64Ty());
    }

    static void addStack(Module &m, StgIRBuilder &builder, Type *elemTy,
                         std::string name, size_t size, Function *&pushFn,
                         Function *&popFn, GlobalVariable *&stack,
                         GlobalVariable *&stackTop) {
        popFn = getOrCreateFunction(
            m, FunctionType::get(elemTy, /*isVarArg=*/false), "pop" + name);
        pushFn =
            getOrCreateFunction(m,
                                FunctionType::get(builder.getVoidTy(), {elemTy},
                                                  /*isVarArg=*/false),
                                "push" + name);
        Type *stackTy = ArrayType::get(elemTy, size);
        // Constant *Init = ConstantAggregateZero::get(stackTy);
        stack = new GlobalVariable(
            m, stackTy, /*isConstant=*/false, GlobalValue::ExternalLinkage,
            /*Initializer=*/ConstantAggregateZero::get(stackTy),
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

Value *materializeAtomInt(const AtomInt *i, StgIRBuilder &builder,
                          BuildCtx &bctx) {
    return builder.getInt64(i->getVal());
}

Value *materializeAtomIdent(const AtomIdent *id, StgIRBuilder &builder,
                            BuildCtx &bctx) {
    assert(false && "umimplemented materialization of identifier atom");
}

Value *materializeAtom(const Atom *a, StgIRBuilder &builder, BuildCtx &bctx) {
    switch (a->getKind()) {
        case Atom::AK_Int:
            return materializeAtomInt(cast<AtomInt>(a), builder, bctx);
        case Atom::AK_Ident:
            return materializeAtomIdent(cast<AtomIdent>(a), builder, bctx);
    }
    assert(false && "unreachable, switch case should have fired");
}

void materializeAp(const ExpressionAp *ap, Module &m, StgIRBuilder &builder,
                   BuildCtx &bctx) {
    for (Atom *p : *ap) {
        Value *v = materializeAtom(p, builder, bctx);
        builder.CreateCall(bctx.pushInt, {v});

        Function *Cont = bctx.getFunctionFromName(ap->getFnName());
        CreateTailCall(builder, Cont, {});
    }
};

void materializeConstructor(const ExpressionConstructor *c, Module &m,
                            StgIRBuilder &builder, BuildCtx &bctx) {
    int TotalSize = 0;
    for (Atom *a : c->args_range()) {
        AtomInt *ai = cast<AtomInt>(a);

        TotalSize += 4;  // bytes.
    }
    Value *rawMem = builder.CreateCall(bctx.malloc,
                                       {builder.getInt64(TotalSize)}, "rawmem");
    Type *structType = bctx.getTypeFromName(c->getName()).second;
    Value *typedMem =
        builder.CreateBitCast(rawMem, structType->getPointerTo(), "typedmem");
    // Push values into the constructed value
    unsigned i = 0;
    for (Atom *a : c->args_range()) {
        AtomInt *ai = cast<AtomInt>(a);
        std::vector<Value *> idxs = {builder.getInt64(0), builder.getInt32(0)};
        Value *indexedMem = builder.CreateGEP(
            typedMem, idxs, "indexedmem_" + std::to_string(i));

        Value *v = materializeAtom(ai, builder, bctx);
        v->setName("param_" + std::to_string(i));
        builder.CreateStore(v, indexedMem);
        i++;
    }
    Value *memAddr =
        builder.CreatePtrToInt(typedMem, builder.getInt64Ty(), "memaddr");
    builder.CreateCall(bctx.pushInt, {memAddr});

    // now pop a continuation off the return stack and invoke it
    CallInst *ReturnCont =
        builder.CreateCall(bctx.popReturnCont, {}, "returncont");
    CreateTailCall(builder, ReturnCont, {});
};

// materialize alternate handling of case `ident` of ...)
Function *materializeCaseConstructorAlt(const ExpressionCase *c, Module &m,
                                        StgIRBuilder &builder, BuildCtx &bctx) {
    const Identifier scrutinee = cast<AtomIdent>(c->getScrutinee())->getIdent();
    Function *handler = getOrCreateFunction(
        m, FunctionType::get(builder.getVoidTy(), {}, /*isVarArg=*/false),
        "case_alt_" + scrutinee);
    BasicBlock *Entry = BasicBlock::Create(m.getContext(), "entry", handler);
    builder.SetInsertPoint(Entry);

    // We can only handle one alt :)
    assert(c->alts_size() == 1);
    for (CaseAlt *a : c->alts_range()) {
        if (CaseAltDestructure *d = dyn_cast<CaseAltDestructure>(a)) {
            // TODO: check that we have the correct destructured value
            // TODO: create Scope :P
            int i = 0;
            DataDeclaration *Decl =
                bctx.getTypeFromName(d->getConstructorName()).first;
            // Declaration and destructuring param sizes should match.
            assert(Decl->types_size() == d->variables_size());
            std::map<Identifier, Value *> DestructuredVals;
            for (Identifier var : d->variables_range()) {
                assert(i == 0);

                if (*Decl->getTypeName(i) == "PrimInt") {
                    DestructuredVals[var] = builder.CreateCall(
                        bctx.popInt, {}, "arg_int_" + std::to_string(i));
                } else {
                    assert(false &&
                           "umimplemented destructuring for non int types");
                }
                materializeExpr(a->getRHS(), m, builder, bctx);
                i++;
            }

        } else {
            assert(false && "unimplemented.");
        }
    }
    builder.CreateRetVoid();
    return handler;
}

// case over a constructor. Note: this is a HACK, this is not how you should
// find out what this is. The correct thing to to is to look at the type
// signature of the scrutinee and then decide what is supposed to happen.
// Right now, I'm only interested in getting my stuff working which is why
// I'm doing it this way.
void materializeCaseConstructor(const ExpressionCase *c, Module &m,
                                StgIRBuilder &builder, BuildCtx &bctx) {
    // NOTE: save insert BB because materializeCaseConstructorAlt changes this.
    BasicBlock *BB = builder.GetInsertBlock();

    const Identifier scrutinee = cast<AtomIdent>(c->getScrutinee())->getIdent();
    // TODO: come up with a notion of scope.
    Function *Next = bctx.getFunctionFromName(scrutinee);
    // push a return continuation for the function `Next` to follow.
    Function *AltHandler = materializeCaseConstructorAlt(c, m, builder, bctx);

    builder.SetInsertPoint(BB);
    builder.CreateCall(bctx.pushReturnCont, {AltHandler});
    CreateTailCall(builder, Next, {});
}

void materializeCase(const ExpressionCase *c, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx) {
    if (isa<AtomInt>(c->getScrutinee())) {
        assert(false && "primitive case unimplemented");
    } else {
        materializeCaseConstructor(c, m, builder, bctx);
    }
}

void materializeExpr(const Expression *e, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx) {
    switch (e->getKind()) {
        case Expression::EK_Ap:
            materializeAp(cast<ExpressionAp>(e), m, builder, bctx);
            break;
        case Expression::EK_Cons:
            materializeConstructor(cast<ExpressionConstructor>(e), m, builder,
                                   bctx);
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
        Elements.push_back(bctx.getTypeFromName(*Name).second);
    }

    StructType *Ty =
        StructType::create(m.getContext(), Elements, decl->getName());
    return Ty;
};

int compile_program(stg::Program *program, int argc, char **argv) {
    cout << "> program: " << *program << "\n";
    LLVMContext ctx;
    StgIRBuilder builder(ctx);
    Module m("Module", ctx);

    BuildCtx bctx(m, builder);

    Binding *entrystg = nullptr;
    for (DataDeclaration *decl : program->declarations_range()) {
        bctx.insertType(decl->getName(), decl,
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

    if (argc != 1) {
        assert(argc == 2);
        std::error_code EC;
        llvm::raw_fd_ostream OS(argv[1], EC, llvm::sys::fs::F_None);
        llvm::WriteBitcodeToFile(&m, OS);
    }
    return 0;
}
