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
    Scope() : inner(None){};

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
            if (It != inner.getValue()->end()) return It;
        }

        return m.find(k);
    }

    const_iterator find(K k) const {
        if (inner) {
            auto It = inner.getValue()->find(k);
            if (It != inner.getValue()->end()) return It;
        }

        return m.find(k);
    }

    void pushScope() {
        if (!inner) {
            inner = Optional<Scope<K, V> *>(new Scope());
        } else {
            inner.getValue()->pushScope();
        }
    }

    void popScope() {
        assert(inner && "calling popScope on the innermost scope");
        if (inner.getValue()->isInnermostScope()) {
            inner = None;
        } else {
            inner.getValue()->popScope();
        }
    }

    bool isInnermostScope() const { return !inner; }
};

static const int stackSize = 5000;
struct BuildCtx {
   public:
    using IdentifierMapTy = Scope<Identifier, Value *>;

    // a map from data constructors to the underlying DataConstructor
    using DataConstructorMap =
        std::map<ConstructorName, std::tuple<DataConstructor *, Type *>>;

    // a map from data types to their underlying DataType.
    using DataTypeMap = std::map<TypeName, DataType *>;

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
        populateIntrinsicTypes(m, builder, dataTypeMap, dataConstructorMap);

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

    // map a binding to a function in the given scope.
    void insertBinding(Binding *b, Function *f) {
        identifiermap.insert(b->getName(), f);
    }

    // map an identifier to a value in the current scope.
    void insertIdentifier(Identifier ident, Value *v) {
        identifiermap.insert(ident, v);
    }

    // lookup an identifier form the current scope
    Value *getIdentifier(Identifier ident) {
        auto It = identifiermap.find(ident);
        assert(It != identifiermap.end());
        return It->second;
    }

    // push a scope for identifier resolution
    void pushScope() { identifiermap.pushScope(); }

    // pop a scope for identifier resolution
    void popScope() { identifiermap.popScope(); }

    Function *getFunctionFromName(std::string name) const {
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

    // TODO: this cannot be structType because we can have things like
    // PrimInt. This is an abuse and I should fix this.
    void insertDataConstructor(std::string name, DataConstructor *cons,
                               Type *type) {
        dataConstructorMap[name] = std::make_pair(cons, type);
    }

    std::pair<DataConstructor *, Type *> getDataConstructorFromName(
        std::string name) const {
        auto It = dataConstructorMap.find(name);
        if (It == dataConstructorMap.end()) {
            errs() << "unknown name: " << name << "\n";
            assert(false && "unknown data constructor name");
        }
        return It->second;
    }
    void insertDataType(std::string name, DataType *datatype) {
        dataTypeMap[name] = datatype;
    }

    DataType *getDataTypeName(std::string name) const {
        auto It = dataTypeMap.find(name);
        if (It == dataTypeMap.end()) {
            errs() << "unknown name: " << name << "\n";
            assert(false && "unknown type name");
        }
        return It->second;
    }

   private:
    IdentifierMapTy identifiermap;
    DataConstructorMap dataConstructorMap;
    DataTypeMap dataTypeMap;

    static void populateIntrinsicTypes(Module &m, StgIRBuilder &builder,
                                       DataTypeMap &typemap,
                                       DataConstructorMap &consmap) {
        DataConstructor *cons = new DataConstructor("PrimInt", {});
        consmap["PrimInt"] = std::make_pair(cons, builder.getInt64Ty());

        DataType *primIntTy = new DataType("PrimInt", {cons});
        typemap["PrimInt"] = primIntTy;
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
    return bctx.getIdentifier(id->getIdent());
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
    // TODO: refactor this to use DataLayout.
    int TotalSize = 8;  // for the tag.
    for (Atom *a : c->args_range()) {
        AtomInt *ai = cast<AtomInt>(a);
        TotalSize += 4;  // bytes.
    }
    DataConstructor *cons;
    Type *structType;

    std::tie(cons, structType) = bctx.getDataConstructorFromName(c->getName());
    Value *rawMem = builder.CreateCall(bctx.malloc,
                                       {builder.getInt64(TotalSize)}, "rawmem");
    Value *typedMem =
        builder.CreateBitCast(rawMem, structType->getPointerTo(), "typedmem");

    const int Tag = cons->getParent()->getIndexForConstructor(cons);
    Value *tagIndex = builder.CreateGEP(
        typedMem, {builder.getInt64(0), builder.getInt32(0)}, "tag_index");
    builder.CreateStore(builder.getInt64(Tag), tagIndex);

    // Push values into the constructed value
    unsigned i = 1;
    for (Atom *a : c->args_range()) {
        AtomInt *ai = cast<AtomInt>(a);
        std::vector<Value *> idxs = {builder.getInt64(0), builder.getInt32(i)};
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

// materialize destructure code for an alt over a constructor.
// Assumes that the builder is focused on the correct basic block.
void materializeCaseConstructorAltDestructure(const ExpressionCase *c,
                                              const CaseAltDestructure *d,
                                              Value *MemAddr, 
                                              Module &m, StgIRBuilder &builder,
                                              BuildCtx &bctx) {
    // TODO: create an RTTI class that does the push/pop automatically
    bctx.pushScope();

    // TODO: check that we have the correct destructured value
    // TODO: create Scope :P
    int i = 0;
    DataType *decl;
    DataConstructor *cons;

    Type *T;
    std::tie(cons, T) =
        bctx.getDataConstructorFromName(d->getConstructorName());
    // a constructor will have a StructType. If not, this deserves
    // to blow up. TODO: make this safe.
    StructType *DeclTy = cast<StructType>(T);

    Value *StructPtr =
        builder.CreateIntToPtr(MemAddr, DeclTy->getPointerTo(), "structptr");

    // Declaration and destructuring param sizes should match.
    assert(cons->types_size() == d->variables_size());
    for (Identifier var : d->variables_range()) {
        // We need i+1 because 0th slot is used for type.
        SmallVector<Value *, 2> Idxs = {builder.getInt64(0),
                                        builder.getInt32(i + 1)};
        Value *Slot =
            builder.CreateGEP(StructPtr, Idxs, "slot_int_" + std::to_string(i));
        Value *V = builder.CreateLoad(Slot, "arg_int_" + std::to_string(i));
        if (*cons->getTypeName(i) == "PrimInt") {
            bctx.insertIdentifier(var, V);
        } else {
            assert(false && "umimplemented destructuring for non int types");
        }
        i++;
    }
    materializeExpr(d->getRHS(), m, builder, bctx);
    bctx.popScope();
}

static const DataType *getCommonDataTypeFromAlts(const ExpressionCase *c,
                                                 const StgIRBuilder &builder,
                                                 const BuildCtx &bctx) {
    const DataType *commondecl = nullptr;
    auto setCommonType = [&](const DataType *newdecl) -> void {
        if (commondecl == nullptr) {
            commondecl = newdecl;
            return;
        };
        assert(commondecl == newdecl &&
               "derived two different data declarations for case");
    };

    for (CaseAlt *a : c->alts_range()) {
        if (CaseAltDestructure *destructure = dyn_cast<CaseAltDestructure>(a)) {
            const DataConstructor *dc =
                std::get<0>(bctx.getDataConstructorFromName(
                    destructure->getConstructorName()));
            setCommonType(dc->getParent());
        } else if (CaseAltInt *i = dyn_cast<CaseAltInt>(a)) {
            assert(false && "unimplemented type deduction");
        } else if (CaseAltVariable *d = dyn_cast<CaseAltVariable>(a)) {
            assert(false &&
                   "unimplemented  type deduction for case alt variable");
        } else {
            assert(false && "unknown case alt.");
        }
    }
    assert(commondecl);
    return commondecl;
};

// materialize alternate handling of case `ident` of ...)
Function *materializeCaseConstructorAlts(const ExpressionCase *c, Module &m,
                                         StgIRBuilder &builder,
                                         BuildCtx &bctx) {
    const Identifier scrutinee = cast<AtomIdent>(c->getScrutinee())->getIdent();
    Function *f = getOrCreateFunction(
        m, FunctionType::get(builder.getVoidTy(), {}, /*isVarArg=*/false),
        "case_alt_" + scrutinee);
    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", f);
    builder.SetInsertPoint(entry);

    const DataType *dataType = getCommonDataTypeFromAlts(c, builder, bctx);

    Value *MemAddr = builder.CreateCall(bctx.popInt, {}, "memaddr");

    Value *TagPtr = builder.CreateIntToPtr(
        MemAddr, builder.getInt64Ty()->getPointerTo(), "tagptr");
    // Since we only care about the tag, we can convert to i64 and forget about
    // the rest.
    Value *Tag = builder.CreateLoad(TagPtr, "tag");

    BasicBlock *failure = BasicBlock::Create(m.getContext(), "failure", f);
    builder.SetInsertPoint(failure);
    builder.CreateUnreachable();

    builder.SetInsertPoint(entry);
    SwitchInst *switch_ = builder.CreateSwitch(
        Tag, failure, /*ncases=*/dataType->constructors_size());

    for (CaseAlt *a : c->alts_range()) {
        switch (a->getKind()) {
            case CaseAlt::CAK_Destructure: {
                CaseAltDestructure *d = cast<CaseAltDestructure>(a);
                BasicBlock *bb = BasicBlock::Create(m.getContext(),
                                                    d->getConstructorName(), f);
                builder.SetInsertPoint(bb);

                const int Tag = dataType->getIndexForConstructor(std::get<0>(
                    bctx.getDataConstructorFromName(d->getConstructorName())));
                // teach the switch case to switch to this BB on encountering the tag.
                switch_->addCase(builder.getInt64(Tag), bb);
                materializeCaseConstructorAltDestructure(c, d, MemAddr, m, builder,
                                                         bctx);
                builder.CreateRetVoid();
                break;
            }
            case CaseAlt::CAK_Int:
                assert(false && "case of a non-int scrutinee cannot have int");
                break;
            case CaseAlt::CAK_Variable:
                assert(false && "unimplemented");
                break;
        }
    }
    return f;
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
    Function *AltHandler = materializeCaseConstructorAlts(c, m, builder, bctx);

    builder.SetInsertPoint(BB);
    builder.CreateCall(bctx.pushReturnCont, {AltHandler});
    CreateTailCall(builder, Next, {});
}

void materializeCase(const ExpressionCase *c, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx) {
    if (isa<AtomInt>(c->getScrutinee())) {
        assert(false && "primitive case unimplemented");
    } else {
        // HACK: Right now, we assume that all non-direct matches are over
        // constructors, this is wrong a f. We should actually look at the
        // type of c->scrutinee and then decide.
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

    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(entry);
    materializeLambda(b->getRhs(), m, builder, bctx);
    builder.CreateRetVoid();
    return F;
}

// construct a StructType for a DataConstructor
StructType *materializeDataConstructor(const DataType *decl,
                                       const DataConstructor *b,
                                       const Module &m, StgIRBuilder &builder,
                                       const BuildCtx &bctx) {
    std::vector<Type *> Elements;
    Elements.push_back(builder.getInt64Ty());  // TAG.
    for (TypeName *Name : b->types_range()) {
        Elements.push_back(get<1>(bctx.getDataConstructorFromName(*Name)));
    }

    StructType *Ty =
        StructType::create(m.getContext(), Elements,
                           decl->getTypeName() + "_variant_" + b->getName());
    return Ty;
};

int compile_program(stg::Program *program, int argc, char **argv) {
    cout << "> program: " << *program << "\n";
    LLVMContext ctx;
    StgIRBuilder builder(ctx);
    Module m("Module", ctx);

    BuildCtx bctx(m, builder);

    Binding *entrystg = nullptr;
    for (DataType *datatype : program->datatypes_range()) {
        assert(datatype->constructors_size() > 0);
        bctx.insertDataType(datatype->getTypeName(), datatype);
        for (DataConstructor *cons : datatype->constructors_range()) {
            bctx.insertDataConstructor(
                cons->getName(), cons,
                materializeDataConstructor(datatype, cons, m, builder, bctx));
        }
    }

    for (Binding *b : program->bindings_range()) {
        if (b->getName() == "main") {
            assert(!entrystg && "program has more than one main.");
            entrystg = b;
        }

        bctx.insertBinding(b, materializeBinding(b, m, builder, bctx));
    }

    m.print(outs(), nullptr);

    if (argc != 1) {
        assert(argc == 2);
        std::error_code EC;
        llvm::raw_fd_ostream OS(argv[1], EC, llvm::sys::fs::F_None);
        llvm::WriteBitcodeToFile(&m, OS);
    }
    return 0;
}
