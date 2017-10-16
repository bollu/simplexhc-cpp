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

enum class ClosureTag {
    FullySaturated = 0,
    FreeBegin,  // closures with free params. For n free params, should be
                // FreeBegin + n.
};

class BuildCtx;

using StgIRBuilder = IRBuilder<>;

void materializeExpr(const Expression *e, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx);

struct LLVMClosureData {
    AssertingVH<Function> fn;
    AssertingVH<GlobalVariable> closure;

    LLVMClosureData(Function *fn, GlobalVariable *closure)
        : fn(fn), closure(closure){};
};
LLVMClosureData materializeBinding(const Binding *b, Module &m,
                                   StgIRBuilder &builder, BuildCtx &bctx);
void materializeLambda(const Lambda *l, Module &m, StgIRBuilder &builder,
                       BuildCtx &bctx);

LLVMClosureData materializeStaticClosureForFn(Function *F,
                                        std::string name, Module &m,
                                        StgIRBuilder &builder, BuildCtx &bctx);


// http://web.iitd.ac.in/~sumeet/flex__bison.pdf
// http://aquamentus.com/flex_bison.html

// Get a function with name `name`, and if it doesn't exist, create
// a function with name `name`, type `FTy`, module `m`.
static AssertingVH<Function> getOrCreateFunction(Module &m, FunctionType *FTy,
                                                 std::string name) {
    Function *F = m.getFunction(name);
    if (F) return F;

    return Function::Create(FTy, GlobalValue::ExternalLinkage, name, &m);
}

static AssertingVH<Function> getOrCreateContFunc(Module &m,
                                                 StgIRBuilder builder,
                                                 std::string name) {
    FunctionType *ContTy = FunctionType::get(builder.getVoidTy(), {}, false);
    return getOrCreateFunction(m, ContTy, name);
}

static CallInst *CreateTailCall(StgIRBuilder &builder, Value *Fn,
                                ArrayRef<Value *> Args,
                                const Twine &Name = "") {
    CallInst *Call = builder.CreateCall(Fn, Args, Name);
    Call->setTailCallKind(CallInst::TCK_MustTail);
    return Call;
}

static Value *TransmuteToCont(Value *V, StgIRBuilder &builder) {
    Type *ContTy = FunctionType::get(builder.getVoidTy(), {}, false);
    return builder.CreateIntToPtr(V, ContTy->getPointerTo(),
                                  V->getName() + "_transmute_to_fn");
}

static Value *TransmuteToInt(Value *V, StgIRBuilder &builder) {
    if (V->getType()->isIntegerTy()) return V;
    return builder.CreatePtrToInt(V, builder.getInt64Ty(),
                                  V->getName() + "_transmute_to_int");
}

static Type *getVoidPointerTy(StgIRBuilder &builder) {
    return builder.getInt8Ty()->getPointerTo();
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

    void replace(K k, V v) {
        if (inner)
            inner.getValue()->replace(k, v);
        else {
            assert(m.find(k) != m.end());
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
            delete inner.getValue();
            inner = None;
        } else {
            inner.getValue()->popScope();
        }
    }

    bool isInnermostScope() const { return !inner; }

    ~Scope() {
        if (inner) delete inner.getValue();
    }
};

static const int STACK_SIZE = 5000;
static const int HEAP_SIZE = 5000;
class BuildCtx {
   public:
    using IdentifierMapTy = Scope<Identifier, AssertingVH<Value>>;

    using BindingMapTy = std::map<Identifier, LLVMClosureData>;

    // a map from data constructors to the underlying DataConstructor
    using DataConstructorMap =
        std::map<ConstructorName, std::tuple<DataConstructor *, Type *>>;

    // a map from data types to their underlying DataType.
    using DataTypeMap = std::map<TypeName, DataType *>;

    // Closure tags to global variables representing ints.
    // TODO: do this when you have more free time :)
    // using ClosureTagMap = std::map<ClosureTag, AssertingVH<GlobalVariable>>;

    AssertingVH<Function> popInt, pushInt;
    LLVMClosureData *printInt;
    AssertingVH<Function> malloc;
    AssertingVH<Function> pushReturnCont, popReturnCont;
    AssertingVH<Function> pushHeap, popHeap;
    AssertingVH<GlobalVariable> stackInt;
    AssertingVH<GlobalVariable> stackIntTop;

    AssertingVH<GlobalVariable> heap;
    AssertingVH<GlobalVariable> heapTop;

    AssertingVH<GlobalVariable> enteringFnAddr;
    AssertingVH<Function> enterDynamicClosure;

    static const unsigned MAX_FREE_PARAMS = 10;
    // type of closure { i64 tag, () -> () fn, <free vars> }
    StructType *ClosureTy[MAX_FREE_PARAMS];
    // type of values in return stack: () -> ()
    Type *ContTy;
    // stack of return values, in reality a large array
    AssertingVH<GlobalVariable> stackReturnCont;
    // pointer to offset to top of return stack.
    AssertingVH<GlobalVariable> stackReturnContTop;

    BuildCtx(Module &m, StgIRBuilder &builder) {
        // *** ContTy ***
        ContTy = FunctionType::get(builder.getVoidTy(), {}, false);

        populateIntrinsicTypes(m, builder, dataTypeMap, dataConstructorMap);


        malloc = getOrCreateFunction(
            m,
            FunctionType::get(builder.getInt8Ty()->getPointerTo(),
                              {builder.getInt64Ty()}, false),
            "malloc");
        // *** Int ***
        addStack(m, builder, builder.getInt64Ty(), "Int", STACK_SIZE, pushInt,
                 popInt, stackInt, stackIntTop);

        // type of returns.
        addStack(m, builder, ContTy->getPointerTo(), "Return", STACK_SIZE,
                 pushReturnCont, popReturnCont, stackReturnCont,
                 stackReturnContTop);

        // *** Heap ***
        addStack(m, builder, builder.getInt64Ty(), "Heap", HEAP_SIZE, pushHeap,
                 popHeap, heap, heapTop);

        // *** enteringFnAddr ***
        enteringFnAddr = new GlobalVariable(
            m, builder.getInt64Ty(), /*isConstant=*/false,
            GlobalValue::ExternalLinkage,
            ConstantInt::get(builder.getInt64Ty(), 0), "enteringFnAddr");

        // ClosureTy
        std::vector<Type *> StructMemberTys;

        StructMemberTys = {builder.getInt64Ty(), ContTy->getPointerTo()};
        ClosureTy[0] = StructType::create(StructMemberTys, "Closure_Free0");
        for (unsigned i = 1; i < MAX_FREE_PARAMS; i++) {
            StructMemberTys = {builder.getInt64Ty(), ContTy,
                               ArrayType::get(getVoidPointerTy(builder), i)};
            ClosureTy[i] = StructType::create(
                StructMemberTys, "Closure_Free" + std::to_string(i));
        }

        // Intrinsics: NOTE: can only be inited after closures have been inited.
        // *** printInt *** //
        printInt = [&] {
            Function *F = getOrCreateFunction(
                m, FunctionType::get(builder.getVoidTy(),
                                     /*isVarArg=*/false), "printInt");
            return new LLVMClosureData(materializeStaticClosureForFn(
                F, "closure_printInt", m, builder, *this));
        }();
        this->staticBindingMap.insert(std::make_pair("printInt", *printInt));

        // *** enter dynamic closure ***
        enterDynamicClosure = addEnterDynamicClosureToModule(m, builder);
    }

    ~BuildCtx() {
        delete this->printInt;
        // delete this->stackReturnCont;
        // delete this->stackReturnContTop;
        // delete this->stackInt;
        // delete this->stackIntTop;
    }

    // map a binding to a function in the given scope.
    void insertBinding(Binding *b, LLVMClosureData bdata) {
        assert(staticBindingMap.find(b->getName()) == staticBindingMap.end());
        // assert(false);
        staticBindingMap.insert(std::make_pair(b->getName(), bdata));
    }

    // map an identifier to a value in the current scope.
    void insertIdentifier(Identifier ident, Value *v) {
        identifiermap.insert(ident, v);
    }

    // replace an identifier that is known to exist. asserts that
    void replaceIdentifier(Identifier ident, Value *v) {
        identifiermap.replace(ident, v);
    }
    // lookup an identifier form the current scope
    AssertingVH<Value> getIdentifier(Identifier ident) {
        auto It = identifiermap.find(ident);
        if (It == identifiermap.end()) {
            cerr << "Unknown identifier: " << ident << "\n";
        }
        assert(It != identifiermap.end());
        return It->second;
    }

    Optional<LLVMClosureData> getStaticClosureDataFromName(std::string name,
                                           StgIRBuilder &builder) const {
        auto It = staticBindingMap.find(name);
        if (It == staticBindingMap.end()) {
            cerr << "unknown closure: " << name << "\n";
            assert(false && "unknown closure");
        }
        return It->second;
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

    // Class to create and destroy a scope with RAII.
    class Scoper {
       public:
        Scoper(BuildCtx &bctx) : bctx(bctx) { bctx.pushScope(); };
        ~Scoper() { bctx.popScope(); }

       private:
        BuildCtx &bctx;
    };

   private:
    friend class Scoper;
    // push a scope for identifier resolution
    void pushScope() { identifiermap.pushScope(); }

    // pop a scope for identifier resolution
    void popScope() { identifiermap.popScope(); }

    IdentifierMapTy identifiermap;
    BindingMapTy staticBindingMap;
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
                         std::string name, size_t size,
                         AssertingVH<Function> &pushFn,
                         AssertingVH<Function> &popFn,
                         AssertingVH<GlobalVariable> &stack,
                         AssertingVH<GlobalVariable> &stackTop) {
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

    static Function *addEnterDynamicClosureToModule(Module &m, StgIRBuilder builder) {
        Function *F = getOrCreateFunction(m, FunctionType::get(builder.getVoidTy(), {}, /*varargs*/ false),
        "enter_dynamic_closure");
        BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
        builder.SetInsertPoint(entry);
        builder.CreateRetVoid();
        return F;
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

// materialize the code to enter into a closure.
void materializeEnterStaticClosure(LLVMClosureData Cls, Module &m, StgIRBuilder &builder,
                      BuildCtx &bctx) {
    // for now, assume that our functions have no free vars.
    Value *F = Cls.fn;
    Value *FnAddr =
        builder.CreatePtrToInt(F, builder.getInt64Ty(), "entering_fn_addr");
    builder.CreateStore(FnAddr, bctx.enteringFnAddr);
    CreateTailCall(builder, F, {});
}

void materializeEnterDynamicClosure(Value *V, Module &m, StgIRBuilder &builder, BuildCtx &bctx) {
    // we need to lookup the free vars dynamically and push it onto the stack.
    CreateTailCall(builder, bctx.enterDynamicClosure, {V});
        assert(false && "entering dynamic closure");
};

// As always, the one who organises things (calls the function) does the work:
// push params in reverse order.
void materializeAp(const ExpressionAp *ap, Module &m, StgIRBuilder &builder,
                   BuildCtx &bctx) {
    for (Atom *p : ap->params_reverse_range()) {
        Value *v = materializeAtom(p, builder, bctx);
        if (!isa<AtomInt>(p)) {
            v = builder.CreatePtrToInt(v, builder.getInt64Ty(),
                                       v->getName() + "_to_int");
        }
        errs() << __LINE__ << "\n";
        errs() << "v: " << *v << "\n";
        builder.CreateCall(bctx.pushInt, {v});
        errs() << __LINE__ << "\n";
    }
    Optional<LLVMClosureData> Cls = bctx.getStaticClosureDataFromName(ap->getFnName(), builder);
    if (Cls) {
        materializeEnterStaticClosure(*Cls, m, builder, bctx);
    } else {
         Value *V = bctx.getIdentifier(ap->getFnName());
         materializeEnterDynamicClosure(V, m, builder, bctx);
    }
};

void materializeConstructor(const ExpressionConstructor *c, Module &m,
                            StgIRBuilder &builder, BuildCtx &bctx) {
    // TODO: refactor this to use DataLayout.
    int TotalSize = 8;  // for the tag.
    for (Atom *a : c->args_range()) {
        // cast<AtomInt>(a);
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
        // AtomInt *ai = cast<AtomInt>(a);
        std::vector<Value *> idxs = {builder.getInt64(0), builder.getInt32(i)};
        Value *indexedMem = builder.CreateGEP(
            typedMem, idxs, "indexedmem_" + std::to_string(i));

        Value *v = materializeAtom(a, builder, bctx);
        v->setName("param_" + std::to_string(i));
        v = TransmuteToInt(v, builder);
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
                                              Value *MemAddr, Module &m,
                                              StgIRBuilder &builder,
                                              BuildCtx &bctx) {
    BuildCtx::Scoper scoper(bctx);

    // TODO: check that we have the correct destructured value
    // TODO: create Scope :P
    int i = 0;
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
}

static const DataType *getCommonDataTypeFromAlts(const ExpressionCase *c,
                                                 const StgIRBuilder &builder,
                                                 const BuildCtx &bctx) {
    errs() << __PRETTY_FUNCTION__ << "\n";
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

    // HACK: special case for case x of { default -> ... }
    if (c->alts_size() == 1 && isa<CaseAltDefault>(*(c->alts_begin()))) {
        const CaseAltDefault *default_ = cast<CaseAltDefault>(*c->alts_begin());
        materializeExpr(default_->getRHS(), m, builder, bctx);
        builder.CreateRetVoid();
        return f;
    }

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
    SwitchInst *switch_ =
        builder.CreateSwitch(Tag, failure, /*ncases=*/c->alts_size());

    for (CaseAlt *a : c->alts_range()) {
        switch (a->getKind()) {
            case CaseAlt::CAK_Destructure: {
                CaseAltDestructure *d = cast<CaseAltDestructure>(a);
                BasicBlock *bb = BasicBlock::Create(m.getContext(),
                                                    d->getConstructorName(), f);
                builder.SetInsertPoint(bb);

                const DataType *dataType =
                    getCommonDataTypeFromAlts(c, builder, bctx);
                const int Tag = dataType->getIndexForConstructor(std::get<0>(
                    bctx.getDataConstructorFromName(d->getConstructorName())));
                // teach the switch case to switch to this BB on encountering
                // the tag.
                switch_->addCase(builder.getInt64(Tag), bb);
                materializeCaseConstructorAltDestructure(c, d, MemAddr, m,
                                                         builder, bctx);
                builder.CreateRetVoid();
                break;
            }
            case CaseAlt::CAK_Int:
                assert(false && "case of a non-int scrutinee cannot have int");
                break;
            case CaseAlt::CAK_Variable:
                assert(false && "unimplemented alt codegen for cak_variable");
                break;
            case CaseAlt::CAK_Default:
                assert(false && "unimplemented alt codegen for cak_default");
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
    // In the case of constructor, the static closure entry _must_ exist.
    Optional<LLVMClosureData> NextCls = bctx.getStaticClosureDataFromName(scrutinee, builder);
    // push a return continuation for the function `Next` to follow.
    Function *AltHandler = materializeCaseConstructorAlts(c, m, builder, bctx);

    builder.SetInsertPoint(BB);
    builder.CreateCall(bctx.pushReturnCont, {AltHandler});
    materializeEnterStaticClosure(*NextCls, m, builder, bctx);
    // CreateTailCall(builder, Next, {});
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

// *** LET CODEGEN
// When someone calls a binding with free variables, the caller will push
// free variables onto the stack first. So, we can pull the free vars
// out from the stack.
//
//
// As always, the one who initiates something must take burden: callee pushes
// stuff onto the stack (for free variables) in the reverse order.
// so, g = let f = \(a b c) (x y z) -> .. in alpha (f) 10 will become:
//
// g:
//     push c
//     push b
//     push a
//     push f <---
//     node = <f's location on stack>
//     enter alpha
//
//
//
// f:
//   pop a
//   pop b
//   pop c
//

// Copied from materializeBinding - consider merging with.
void materializeLetBinding(Function *F, const Binding *b, Module &m,
                           StgIRBuilder &builder, BuildCtx &bctx) {
    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(entry);

    // loop over the free parameters. These we would have in the scope
    // of our let function, but we need to use the closure struct to get
    // access to them.
    BuildCtx::Scoper s(bctx);
    int i = 0;

    for (const Parameter *p : b->getRhs()->free_params_range()) {
        // HACK,WRONG: it should index the heap
        Value *Idx =
            builder.CreateGEP(bctx.enteringFnAddr, {builder.getInt64(i + 1)},
                              "free_" + p->getName() + "_idx");
        Value *v = builder.CreateLoad(Idx, "free_" + p->getName());
        bctx.insertIdentifier(p->getName(), v);
        i++;
    }

    materializeLambda(b->getRhs(), m, builder, bctx);
    builder.CreateRetVoid();
}
void materializeLet(const ExpressionLet *l, Module &m, StgIRBuilder &builder,
                    BuildCtx &bctx) {
    BasicBlock *Entry = builder.GetInsertBlock();

    // Open a new scope.---
    BuildCtx::Scoper scoper(bctx);
    // 1. Create stubs for all bindings.
    std::map<Binding *, Function *> bindingToEntryFunc;
    for (Binding *b : l->bindings_range()) {
        Function *bf = getOrCreateContFunc(m, builder, b->getName());
        bindingToEntryFunc[b] = bf;
        bctx.insertIdentifier(b->getName(), bf);
    }

    // 2. Fill in stubs
    for (auto It : bindingToEntryFunc) {
        Function *bf = It.second;
        const Binding *b = It.first;
        materializeLetBinding(bf, b, m, builder, bctx);
    }

    // 3. Create Heap closures for all stub bindings, create a scope
    // for the RHS to use.
    builder.SetInsertPoint(Entry);
    for (auto It : bindingToEntryFunc) {
        Function *bf = It.second;
        const Binding *b = It.first;
        Value *HeapTop = builder.CreateLoad(
            bctx.heapTop, "heap_top_" + b->getName() + "_loc");
        HeapTop = TransmuteToCont(HeapTop, builder);

        Value *bfTransmuted = TransmuteToInt(bf, builder);
        builder.CreateCall(bctx.pushHeap, {bfTransmuted});

        // push heap values
        for (const Parameter *p : b->getRhs()->free_params_range()) {
            Value *v = bctx.getIdentifier(p->getName());
            builder.CreateCall(bctx.pushHeap, {v});
        }
        bctx.replaceIdentifier(b->getName(), HeapTop);
    }

    // Now create heap locations for these bad boys and
    // set those heap locations to be the "correct"
    // locations.

    materializeExpr(l->getRHS(), m, builder, bctx);
};

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
        case Expression::EK_Let:
            materializeLet(cast<ExpressionLet>(e), m, builder, bctx);
            break;
    };
}

void materializeLambda(const Lambda *l, Module &m, StgIRBuilder &builder,
                       BuildCtx &bctx) {
    BuildCtx::Scoper scoper(bctx);
    for (const Parameter *p : l->bound_params_range()) {
        if (p->getType() == "PrimInt") {
            assert(false && "unhandled, functions taking prim ints as params");
        } else {
            Value *pv =
                builder.CreateCall(bctx.popInt, {}, "param_" + p->getName());
            bctx.insertIdentifier(p->getName(), pv);
        }
    }
    materializeExpr(l->getRhs(), m, builder, bctx);
}

LLVMClosureData materializeStaticClosureForFn(Function *F,
                                        std::string name, Module &m,
                                        StgIRBuilder &builder, BuildCtx &bctx) {
    StructType *closureTy = bctx.ClosureTy[0];

    // 2. Create the initializer for the closure
    Constant *initializer = [&] {
        std::vector<Constant *> initializer_list;
        // tag.
        initializer_list.push_back(ConstantInt::get(
            builder.getInt64Ty(), (unsigned)ClosureTag::FreeBegin));
        // function (to jump into)
        initializer_list.push_back(F);

        // assert(nFreeVars == 0 && "free variables not supported yet.");
        return ConstantStruct::get(closureTy, initializer_list);
    }();

    errs() << "initializer: " << *initializer << "\n";

    GlobalVariable *closure =
        new GlobalVariable(m, closureTy, /*isconstant=*/true,
                           GlobalValue::ExternalLinkage, initializer, name);
    return LLVMClosureData(F, closure);
}

LLVMClosureData materializeBinding(const Binding *b, Module &m,
                                   StgIRBuilder &builder, BuildCtx &bctx) {
    FunctionType *FTy =
        FunctionType::get(builder.getVoidTy(), /*isVarArg=*/false);
    Function *F =
        Function::Create(FTy, GlobalValue::ExternalLinkage, b->getName(), &m);

    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(entry);
    materializeLambda(b->getRhs(), m, builder, bctx);
    builder.CreateRetVoid();

    return materializeStaticClosureForFn(F, b->getName() + "_closure", m,
                                   builder, bctx);
}

// construct a StructType for a DataConstructor
StructType *materializeDataConstructor(const DataType *decl,
                                       const DataConstructor *b,
                                       const Module &m, StgIRBuilder &builder,
                                       const BuildCtx &bctx) {
    std::vector<Type *> Elements;
    Elements.push_back(builder.getInt64Ty());  // TAG.
    for (TypeName *Name : b->types_range()) {
        // HACK:
        Elements.push_back(builder.getInt64Ty());
        // Elements.push_back(bctx.getDataTypeName(*Name));
    }

    StructType *Ty =
        StructType::create(m.getContext(), Elements,
                           decl->getTypeName() + "_variant_" + b->getName());
    return Ty;
};

int compile_program(stg::Program *program, int argc, char **argv) {
    cout << "> program: " << *program << "\n";
    static LLVMContext ctx;
    static StgIRBuilder builder(ctx);

    Module *m = new Module("Module", ctx);
    BuildCtx bctx(*m, builder);
    Binding *entrystg = nullptr;
    for (DataType *datatype : program->datatypes_range()) {
        assert(datatype->constructors_size() > 0);
        bctx.insertDataType(datatype->getTypeName(), datatype);
        for (DataConstructor *cons : datatype->constructors_range()) {
            bctx.insertDataConstructor(
                cons->getName(), cons,
                materializeDataConstructor(datatype, cons, *m, builder, bctx));
        }
    }

    for (Binding *b : program->bindings_range()) {
        if (b->getName() == "main") {
            assert(!entrystg && "program has more than one main.");
            entrystg = b;
        }

        bctx.insertBinding(b, materializeBinding(b, *m, builder, bctx));
    }

    if (verifyModule(*m, nullptr) == 1) {
        cerr << "-----\n";
        cerr << "Module:\b";
        errs() << *m << "\n";
        cerr << "-----\n";
        cerr << " *** Broken module found, aborting compilation.\nError:\n";
        verifyModule(*m, &errs());
        return 1;
    }

    m->print(outs(), nullptr);

    if (argc != 1) {
        assert(argc == 2);
        std::error_code EC;
        llvm::raw_fd_ostream OS(argv[1], EC, llvm::sys::fs::F_None);
        llvm::WriteBitcodeToFile(m, OS);
    }
    return 0;
}
