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
#include <sstream>

using namespace std;
using namespace stg;
using namespace llvm;

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

struct LLVMValueData {
    AssertingVH<Value> v;
    DataType *stgtype;

    LLVMValueData(Value *v, DataType *stgtype) : v(v), stgtype(stgtype) {}

    static LLVMValueData createPrimInt(Value *v) {
        return LLVMValueData(v, DataType::createPrimIntTy());
    };
};

LLVMClosureData materializeTopLevelStaticBinding(const Binding *b, Module &m,
                                                 StgIRBuilder &builder,
                                                 BuildCtx &bctx);

void materializeLambda(const Lambda *l, Module &m, StgIRBuilder &builder,
                       BuildCtx &bctx);

LLVMClosureData materializeStaticClosureForFn(Function *F, std::string name,
                                              Module &m, StgIRBuilder &builder,
                                              BuildCtx &bctx);


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

static AssertingVH<Function> createNewFunction(Module &m, FunctionType *FTy,
                                               std::string name) {
    Function *F = m.getFunction(name);
    if (F) {
        errs() << "Function with name:(" << name << ") already exists:\n";
        F->print(errs());
        errs() << "\n";
        assert(false && "function with name already exists.\n");
    }

    return Function::Create(FTy, GlobalValue::ExternalLinkage, name, &m);
}

/*
static AssertingVH<Function> getOrCreateContFunc(Module &m,
                                                 StgIRBuilder builder,
                                                 std::string name) {
    FunctionType *ContTy = FunctionType::get(builder.getVoidTy(), {}, false);
    return getOrCreateFunction(m, ContTy, name);
}
*/

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
                                  V->getName() + "_transmute_to_cont");
}

static Value *TransmuteToInt(Value *V, StgIRBuilder &builder) {
    if (V->getType()->isIntegerTy()) return V;
    return builder.CreatePtrToInt(V, builder.getInt64Ty(),
                                  V->getName() + "_transmute_to_int");
}

// RawMem == void * (in C) == char * == i8* (in LLVM)
static Type *getRawMemTy(StgIRBuilder &builder) {
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
            m.insert(std::make_pair(k, v));
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

    template <typename FTy>
    void dump(FTy F, unsigned nesting = 0) const {
        errs() << "Child:\n";
        if (inner) {
            inner.getValue()->dump(F, nesting + 1);
        }
        errs() << "identifiers(child level=" << nesting << "): \n";
        for (auto It : this->m) {
            F(It.first, It.second, nesting);
        }
    }

    ~Scope() {
        if (inner) delete inner.getValue();
    }
};

static const int STACK_SIZE = 5000;
static const int HEAP_SIZE = 5000;

class BuildCtx {
   public:
    using IdentifierMapTy = Scope<Identifier, LLVMValueData>;

    using StaticBindingMapTy = std::map<Identifier, LLVMClosureData>;

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
    LLVMClosureData *primMultiply;
    AssertingVH<Function> malloc;
    AssertingVH<Function> pushReturnCont, popReturnCont;
    AssertingVH<Function> pushHeap, popHeap;
    AssertingVH<GlobalVariable> stackInt;
    AssertingVH<GlobalVariable> stackIntTop;

    AssertingVH<GlobalVariable> heap;
    AssertingVH<GlobalVariable> heapTop;

    AssertingVH<GlobalVariable> enteringClosureAddr;
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

        populateIntrinsicTypes(m, builder, dataTypeMap, dataConstructorMap,
                               primIntTy, boxedTy);

        malloc = createNewFunction(
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

        // *** enteringClosureAddr ***
        enteringClosureAddr = new GlobalVariable(
            m, builder.getInt64Ty(), /*isConstant=*/false,
            GlobalValue::ExternalLinkage,
            ConstantInt::get(builder.getInt64Ty(), 0), "enteringClosureAddr");

        // ClosureTy
        std::vector<Type *> StructMemberTys;

        StructMemberTys = {ContTy->getPointerTo()};
        ClosureTy[0] = StructType::create(StructMemberTys, "Closure_Free0");
        for (unsigned i = 1; i < MAX_FREE_PARAMS; i++) {
            StructMemberTys = {ContTy->getPointerTo(),
                               ArrayType::get(builder.getInt64Ty(), i)};
            ClosureTy[i] = StructType::create(
                StructMemberTys, "Closure_Free" + std::to_string(i));
        }

        // Intrinsics: NOTE: can only be inited after closures have been inited.
        // *** printInt *** //
        printInt = [&] {
            Function *F =
                createNewFunction(m,
                                  FunctionType::get(builder.getVoidTy(),
                                                    /*isVarArg=*/false),
                                  "printInt");
            return new LLVMClosureData(materializeStaticClosureForFn(
                F, "closure_printInt", m, builder, *this));
        }();
        this->staticBindingMap.insert(std::make_pair("printInt", *printInt));
        this->identifiermap.insert("printInt", LLVMValueData(printInt->closure, this->boxedTy));

        // *** primMultiply *** //
        createPrimFunction(m, builder, *this, "primMultiply", this->primIntTy,
                           this->staticBindingMap, this->identifiermap);

        // *** enter dynamic closure ***
        enterDynamicClosure = addEnterDynamicClosureToModule(m, builder, *this);
    }

    ~BuildCtx() {
        delete this->printInt;
        // delete this->stackReturnCont;
        // delete this->stackReturnContTop;
        // delete this->stackInt;
        // delete this->stackIntTop;
    }

    // Check if a type is that of PrimInt.
    bool isPrimIntTy(const DataType *Ty) const { return Ty == this->primIntTy; }

    // Return the PrimInt type.
    const DataType *getPrimIntTy() const { return this->primIntTy; }

    // map a binding to a function in the given scope.
    void insertTopLevelBinding(Binding *b, LLVMClosureData bdata) {
        assert(staticBindingMap.find(b->getName()) == staticBindingMap.end());
        // assert(false);
        staticBindingMap.insert(std::make_pair(b->getName(), bdata));

        DataType *returnTy =
            this->getDataTypeFromName(b->getRhs()->getReturnTypeName());
        LLVMValueData vdata(&*bdata.closure, returnTy);
        identifiermap.insert(b->getName(), vdata);
    }

    // map an identifier to a value in the current scope.
    void insertIdentifier(Identifier ident, LLVMValueData v) {
        identifiermap.insert(ident, v);
    }

    // lookup an identifier form the current scope
    LLVMValueData getIdentifier(Identifier ident) {
        auto It = identifiermap.find(ident);
        if (It == identifiermap.end()) {
            cerr << "Unknown identifier: " << ident << "\n";
            identifiermap.dump([&](const Identifier &id,
                                   const LLVMValueData &vdata,
                                   unsigned nesting) {
                errs() << id << " => " << *vdata.v << "\n";
            });
            assert(false && "unable to find identifier");
        }
        assert(It != identifiermap.end());
        return It->second;
    }

    Optional<LLVMClosureData> getTopLevelBindingFromName(
            std::string name) const {
        auto It = staticBindingMap.find(name);
        if (It == staticBindingMap.end()) {
            return None;
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

    DataType *getDataTypeFromName(std::string name) const {
        auto It = dataTypeMap.find(name);
        if (It != dataTypeMap.end()) {
            return It->second;
        }
        auto IdIt = identifiermap.find(name);
        if (IdIt != identifiermap.end()) {
            return IdIt->second.stgtype;
        }

        errs() << "Unknown name for data type: " << name << "\n";
        assert(false && "unknown type name");
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
    StaticBindingMapTy staticBindingMap;
    DataConstructorMap dataConstructorMap;
    DataTypeMap dataTypeMap;

    DataType *primIntTy;
    DataType *boxedTy;

    static void populateIntrinsicTypes(Module &m, StgIRBuilder &builder,
                                       DataTypeMap &typemap,
                                       DataConstructorMap &consmap,
                                       DataType *&primIntTy,
                                       DataType *&boxedTy) {
        // primInt
        DataConstructor *cons = new DataConstructor("PrimInt", {});
        consmap["PrimInt"] = std::make_pair(cons, builder.getInt64Ty());

        primIntTy = new DataType("PrimInt", {cons});
        typemap["PrimInt"] = primIntTy;

        // Boxed
        boxedTy = new DataType("Boxed", {});  // void has no constructors.
        typemap["Boxed"] = boxedTy;
    }

    static void addStack(Module &m, StgIRBuilder &builder, Type *elemTy,
                         std::string name, size_t size,
                         AssertingVH<Function> &pushFn,
                         AssertingVH<Function> &popFn,
                         AssertingVH<GlobalVariable> &stack,
                         AssertingVH<GlobalVariable> &stackTop) {
        popFn = createNewFunction(
            m, FunctionType::get(elemTy, /*isVarArg=*/false), "pop" + name);
        pushFn =
            createNewFunction(m,
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

    static Function *addEnterDynamicClosureToModule(Module &m,
                                                    StgIRBuilder builder,
                                                    BuildCtx &bctx) {
        Function *F = createNewFunction(
            m,
            FunctionType::get(builder.getVoidTy(), {builder.getInt64Ty()},
                              /*varargs = */ false),
            "enter_dynamic_closure");

        BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
        builder.SetInsertPoint(entry);
        Argument *closureAddr = &*F->arg_begin();
        closureAddr->setName("closure_addr");

        Value *closureStruct = builder.CreateIntToPtr(closureAddr, bctx.ClosureTy[0]->getPointerTo());
        Value *contSlot = builder.CreateGEP(closureStruct, {builder.getInt64(0), builder.getInt32(0)}, "cont_slot");
        Value *cont = builder.CreateLoad(contSlot, "cont");
        // Value *contAddr = builder.CreatePtrToInt(cont, builder.getInt64Ty(), "cont_addr");

        // store the address of the closure we are entering
        builder.CreateStore(closureAddr, bctx.enteringClosureAddr);
        // call the function
        builder.CreateCall(cont, {});
        builder.CreateRetVoid();
        return F;
    }

    LLVMClosureData
    createPrimFunction(Module &m, StgIRBuilder &builder, const BuildCtx &bctx,
                       std::string fnName,
                       DataType *returnType,
                       BuildCtx::StaticBindingMapTy &staticBindingMap,
                       BuildCtx::IdentifierMapTy &identifiermap) {
      Function *F = createNewFunction(m,
                                      FunctionType::get(builder.getVoidTy(),
                                                        /*isVarArg=*/false),
                                      fnName);
      LLVMClosureData data(materializeStaticClosureForFn(
          F, "closure_" + fnName, m, builder, *this));
      staticBindingMap.insert(std::make_pair(fnName, data));
      identifiermap.insert(fnName, LLVMValueData(data.closure, returnType));
      return data;

    }
};

Value *materializeAtomInt(const AtomInt *i, StgIRBuilder &builder,
                          BuildCtx &bctx) {
    return builder.getInt64(i->getVal());
}

Value *materializeAtomIdent(const AtomIdent *id, StgIRBuilder &builder,
                            BuildCtx &bctx) {
    return bctx.getIdentifier(id->getIdent()).v;
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

void materializeEnterDynamicClosure(Value *V, Module &m, StgIRBuilder &builder,
                                    BuildCtx &bctx) {
    V = TransmuteToInt(V, builder);
    builder.CreateCall(bctx.enterDynamicClosure, {V});
    // assert(false && "entering dynamic closure");
};

// As always, the one who organises things (calls the function) does the
// work: push params in reverse order.
void materializeAp(const ExpressionAp *ap, Module &m, StgIRBuilder &builder,
                   BuildCtx &bctx) {
    BasicBlock *insertBB = builder.GetInsertBlock();
    for (Atom *p : ap->params_reverse_range()) {
        Value *v = materializeAtom(p, builder, bctx);
        if (!isa<AtomInt>(p)) {
            v = builder.CreatePtrToInt(v, builder.getInt64Ty(),
                                       v->getName() + "_to_int");
        }
        builder.CreateCall(bctx.pushInt, {v});
    }
    Value *V = bctx.getIdentifier(ap->getFnName()).v;
    materializeEnterDynamicClosure(V, m, builder, bctx);

    builder.SetInsertPoint(insertBB);
    builder.CreateRetVoid();
};

void materializeConstructor(const ExpressionConstructor *c, Module &m,
                            StgIRBuilder &builder, BuildCtx &bctx) {
    // TODO: refactor this to use DataLayout.
    DataConstructor *cons;
    Type *structType;

    std::tie(cons, structType) = bctx.getDataConstructorFromName(c->getName());

    const int TotalSize =
        m.getDataLayout().getTypeAllocSize(structType);  // for the tag.

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
    builder.CreateRetVoid();
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
        if (cons->getTypeName(i) == "PrimInt") {
            bctx.insertIdentifier(var, LLVMValueData::createPrimInt(V));
        } else {
            DataType *Ty = bctx.getDataTypeFromName(cons->getTypeName(i));
            bctx.insertIdentifier(var, LLVMValueData(V, Ty));
            errs() << __PRETTY_FUNCTION__ << ":" << __LINE__
                   << " inserted value corresponding to int as an expr\n";
        }
        i++;
    }
    materializeExpr(d->getRHS(), m, builder, bctx);
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
Function *materializeCaseConstructorReturnFrame(const ExpressionCase *c,
                                                Module &m,
                                                StgIRBuilder builder,
                                                BuildCtx &bctx) {
    //const Identifier scrutinee = cast<AtomIdent>(c->getScrutinee())->getIdent();
    std::stringstream scrutineeNameSS;
    scrutineeNameSS << *c->getScrutinee();
    Function *f = createNewFunction(
        m, FunctionType::get(builder.getVoidTy(), {}, /*isVarArg=*/false),
        "case_alt_" + scrutineeNameSS.str());
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
    // Since we only care about the tag, we can convert to i64 and forget
    // about the rest.
    Value *Tag = builder.CreateLoad(TagPtr, "tag");

    BasicBlock *failure = BasicBlock::Create(m.getContext(), "failure", f);
    builder.SetInsertPoint(failure);
    Function *trap = getOrCreateFunction(
        m, FunctionType::get(builder.getVoidTy(), {}), "llvm.trap");
    builder.CreateCall(trap, {});
    builder.CreateRetVoid();
    // builder.CreateUnreachable();

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
                // teach the switch case to switch to this BB on
                // encountering the tag.
                switch_->addCase(builder.getInt64(Tag), bb);
                materializeCaseConstructorAltDestructure(c, d, MemAddr, m,
                                                         builder, bctx);
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

// Materialize a case over Prim int.
Function *materializePrimitiveCaseReturnFrame(const ExpressionCase *c, Module &m,
                                         StgIRBuilder builder,
                                         BuildCtx &bctx) {
    std::stringstream namess;
    namess << *c->getScrutinee();

    Function *F = createNewFunction(m, FunctionType::get(builder.getVoidTy(),
                                                         {}, /*isVarArg=*/false),
                                                         namess.str());
    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(entry);

    Value *scrutinee = builder.CreateCall(bctx.popInt, {}, "scrutinee");
    BasicBlock *defaultBB = BasicBlock::Create(m.getContext(), "alt_default", F);
    SwitchInst *switchInst = builder.CreateSwitch(scrutinee, defaultBB);


    ExpressionCase::const_iterator it = c->alts_begin();
    int i = 0;
    for (; it != c->alts_end(); it++, i++) {
        const CaseAlt *alt = *it;
        switch (alt->getKind()) {
            case CaseAlt::CAK_Destructure:
                cerr << "found Destructure case for PrimInt: " << *c;
                assert(false && "cannot destructure a primitive int.");
                report_fatal_error("cannot destructure a primitive int.");
                break;
            case CaseAlt::CAK_Default:
                break;

            case CaseAlt::CAK_Variable:
                break;

            case CaseAlt::CAK_Int: {
                const CaseAltInt *ci = cast<CaseAltInt>(alt);
                BasicBlock *BB = BasicBlock::Create(
                    m.getContext(), "alt_" + std::to_string(ci->getLHS()),
                    entry->getParent());
                switchInst->addCase(builder.getInt64(ci->getLHS()), BB);
                // insert code for expression in the new BB.
                builder.SetInsertPoint(BB);
                materializeExpr(ci->getRHS(), m, builder, bctx);
                //create ret void.
                builder.SetInsertPoint(BB);
               // builder.CreateRetVoid();
                break;
            }
        }
    }


    // we care about the default case
    if (const CaseAltDefault *cd = c->getDefaultAlt()) {
        builder.SetInsertPoint(defaultBB);
        // push the scrutinee back because this is the "default" version.
        builder.CreateCall(bctx.pushInt, {scrutinee});
        materializeExpr(cd->getRHS(), m, builder, bctx);
        //create ret void.
        builder.SetInsertPoint(defaultBB);

        assert(false && "unhandled default");
    }
    else if (const CaseAltVariable *cv = c->getVariableAlt()) {
        defaultBB->setName("var_" + cv->getLHS());
        BuildCtx::Scoper s(bctx);
        builder.SetInsertPoint(defaultBB);
        // create a binding between the scrutinee and the variable name of the alt.
        bctx.insertIdentifier(cv->getLHS(), LLVMValueData::createPrimInt(scrutinee));
        materializeExpr(cv->getRHS(), m, builder, bctx);
        //create ret void.

    }
    else {
        builder.SetInsertPoint(defaultBB);
        Function *trap = getOrCreateFunction(
                m, FunctionType::get(builder.getVoidTy(), {}), "llvm.trap");
        builder.CreateCall(trap, {});
    }

    return F;
}

static const DataType *getTypeOfExpression(const Expression *e,
                                     const BuildCtx &bctx) {
  switch (e->getKind()) {
  case Expression::EK_Ap: {
    const ExpressionAp *ap = cast<ExpressionAp>(e);
    const std::string fnName = ap->getFnName();
    return bctx.getDataTypeFromName(fnName);
    break;
  }
  case Expression::EK_IntLiteral: {
    return bctx.getPrimIntTy();
    break;
  }
  case Expression::EK_Case:
  case Expression::EK_Cons:
  case Expression::EK_Let:
    assert(false && "unimplemented getTypeOfExpression");
  }
}
void materializeCase(const ExpressionCase *c, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx) {
    BasicBlock *insertBlock = builder.GetInsertBlock();
    const Expression *scrutinee = c->getScrutinee();
    const DataType *scrutineety = getTypeOfExpression(scrutinee, bctx);
    if (bctx.isPrimIntTy(scrutineety)) {
        Function *continuation = materializePrimitiveCaseReturnFrame(c, m, builder, bctx);
        Value *clsRaw = builder.CreateCall(bctx.malloc, {builder.getInt64(m.getDataLayout().getTypeAllocSize(bctx.ClosureTy[0]))}, "closure_raw");
        Value *clsTyped = builder.CreateBitCast(clsRaw, bctx.ClosureTy[0]->getPointerTo(), "closure_typed");
        Value *fnSlot = builder.CreateGEP(clsTyped, {builder.getInt64(0), builder.getInt32(0)}, "fn_slot");
        builder.CreateStore(continuation, fnSlot);
        Value *clsAsContHack = builder.CreateBitCast(clsTyped, bctx.ContTy->getPointerTo(), "closure_as_cont_omg_this_is_a_hack_change_type_of_return_stack");

        builder.CreateCall(bctx.pushReturnCont, {clsAsContHack});
        materializeExpr(scrutinee, m, builder, bctx);
        builder.CreateRetVoid();
    }
    else {
        Function *continuation = materializeCaseConstructorReturnFrame(c, m, builder, bctx);
        builder.CreateCall(bctx.pushReturnCont, {continuation});
        materializeExpr(scrutinee, m, builder, bctx);
    }
}

// *** LET CODEGEN
// When someone calls a binding with free variables, the caller will push
// free variables onto the stack first. So, we can pull the free vars
// out from the stack.
//
//
// As always, the one who initiates something must take burden: callee
// pushes stuff onto the stack (for free variables) in the reverse order.
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

// Copied from materializeTopLevelStaticBinding - consider merging with.
Value *_allocateLetBindingDynamicClosure(const Binding *b, BasicBlock *BB,
                                         Module &m, StgIRBuilder builder,
                                         BuildCtx &bctx) {
    const int nFreeParams = b->getRhs()->free_params_size();
    assert(nFreeParams < bctx.MAX_FREE_PARAMS);

    Type *closureTy = bctx.ClosureTy[nFreeParams];
    builder.SetInsertPoint(BB);

    const uint64_t sizeInBytes = m.getDataLayout().getTypeAllocSize(closureTy);
    Value *rawMem = builder.CreateCall(
        bctx.malloc, {builder.getInt64(sizeInBytes)}, "rawmem");
    Value *typedMem = builder.CreateBitCast(rawMem, closureTy->getPointerTo(),
                                            "closure_" + b->getName());
    // no store to function slot, args. These come later.
    return typedMem;
}

// Materialize the function that gets executed when a let-binding is
// evaluated. NOTE: this is exactly the same thing as
// materializeTopLevelStaticBinding, except that it also creates a static
// closure. Consider mergining with materializeTopLevelStaticBinding
Function *_materializeDynamicLetBinding(const Binding *b, Module &m,
                                        StgIRBuilder builder, BuildCtx &bctx) {
    FunctionType *FTy =
        FunctionType::get(builder.getVoidTy(), /*isVarArg=*/false);
    Function *F =
        Function::Create(FTy, GlobalValue::ExternalLinkage, b->getName(), &m);

    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(entry);

    Value *closureAddr =
        builder.CreateLoad(bctx.enteringClosureAddr, "closure_addr_int");
    Value *closure = builder.CreateIntToPtr(
        closureAddr,
        bctx.ClosureTy[b->getRhs()->free_params_size()]->getPointerTo(),
        "closure_typed");
    int i = 0;
    BuildCtx::Scoper s(bctx);
    for (Parameter *p : b->getRhs()->free_params_range()) {
        Value *v = builder.CreateGEP(
            closure,
            {builder.getInt64(0), builder.getInt32(1), builder.getInt32(i)},
            p->getName() + "slot");
        v = builder.CreateLoad(v, p->getName());
        DataType *ty = bctx.getDataTypeFromName(p->getTypeName());
        bctx.insertIdentifier(p->getName(), LLVMValueData(v, ty));
        // builder.createGEP(bctx.enter
        i++;
    }
    materializeLambda(b->getRhs(), m, builder, bctx);
    return F;
}

void materializeLet(const ExpressionLet *l, Module &m, StgIRBuilder &builder,
                    BuildCtx &bctx) {
    BasicBlock *Entry = builder.GetInsertBlock();

    // Open a new scope.---
    BuildCtx::Scoper scoper(bctx);

    // allocate memory slots for each binding.
    // We do this first so that mutually recursive let bindings will now
    // have pointers to each others closures when we start codegen
    for (Binding *b : l->bindings_range()) {
        Value *cls =
            _allocateLetBindingDynamicClosure(b, Entry, m, builder, bctx);
        DataType *Ty =
            bctx.getDataTypeFromName(b->getRhs()->getReturnTypeName());
        // So, because function applications are _fully saturated_, I can
        // consider the type of a let-binding to be the return type. Is this
        // cheating? Sure.
        assert(true && "what type do I assign to a let-binding?");
        bctx.insertIdentifier(b->getName(), LLVMValueData(cls, Ty));
    }

    for (Binding *b : l->bindings_range()) {
        Function *f = _materializeDynamicLetBinding(b, m, builder, bctx);
        Value *cls = bctx.getIdentifier(b->getName()).v;
        // store this in the closure slot.
        Value *fnSlot =
            builder.CreateGEP(cls, {builder.getInt64(0), builder.getInt32(0)},
                              b->getName() + "_fn_slot");
        builder.CreateStore(f, fnSlot);

        int i = 0;
        for (Parameter *p : b->getRhs()->free_params_range()) {
            Value *freeParamSlot = builder.CreateGEP(
                cls,
                {builder.getInt64(0), builder.getInt32(1), builder.getInt32(i)},
                b->getName() + "_free_param_" + p->getName() + "_slot");
            Value *v = bctx.getIdentifier(p->getName()).v;
            v = TransmuteToInt(v, builder);
            builder.CreateStore(v, freeParamSlot);
            i++;
        }
    }


    // Now create heap locations for these bad boys and
    // set those heap locations to be the "correct"
    // locations.
    materializeExpr(l->getRHS(), m, builder, bctx);
};

void materializeExprIntLiteral(const ExpressionIntLiteral *e, Module &m,
                               StgIRBuilder &builder, BuildCtx &bctx) {
    builder.CreateCall(bctx.pushInt, {builder.getInt64(e->getValue())});
    Value *cont = builder.CreateCall(bctx.popReturnCont, {}, "return_cont_" + std::to_string(e->getValue()));
    materializeEnterDynamicClosure(cont, m, builder, bctx);
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
        case Expression::EK_Let:
            materializeLet(cast<ExpressionLet>(e), m, builder, bctx);
            break;
        case Expression::EK_IntLiteral:
            materializeExprIntLiteral(cast<ExpressionIntLiteral>(e), m, builder,
                                      bctx);
    };
}

void materializeLambda(const Lambda *l, Module &m, StgIRBuilder &builder,
                       BuildCtx &bctx) {
    BuildCtx::Scoper scoper(bctx);
    for (const Parameter *p : l->bound_params_range()) {
        if (p->getTypeName() == "PrimInt") {
            assert(false && "unhandled, functions taking prim ints as params");
        } else {
            Value *pv =
                builder.CreateCall(bctx.popInt, {}, "param_" + p->getName());
            DataType *Ty = bctx.getDataTypeFromName(p->getTypeName());
            bctx.insertIdentifier(p->getName(), LLVMValueData(pv, Ty));
        }
    }
    materializeExpr(l->getRhs(), m, builder, bctx);
}

LLVMClosureData materializeStaticClosureForFn(Function *F, std::string name,
                                              Module &m, StgIRBuilder &builder,
                                              BuildCtx &bctx) {
    StructType *closureTy = bctx.ClosureTy[0];

    // 2. Create the initializer for the closure
    Constant *initializer = [&] {
        // assert(nFreeVars == 0 && "free variables not supported yet.");
        return ConstantStruct::get(closureTy, {F});
    }();

    GlobalVariable *closure =
        new GlobalVariable(m, closureTy, /*isconstant=*/true,
                           GlobalValue::ExternalLinkage, initializer, name);
    return LLVMClosureData(F, closure);
}

LLVMClosureData materializeTopLevelStaticBinding(const Binding *b, Module &m,
                                                 StgIRBuilder &builder,
                                                 BuildCtx &bctx) {
    assert(b->getRhs()->free_params_size() == 0 &&
           "top level bindings cannot have any free paramters.");
    FunctionType *FTy =
        FunctionType::get(builder.getVoidTy(), /*isVarArg=*/false);
    Function *F =
        Function::Create(FTy, GlobalValue::ExternalLinkage, b->getName(), &m);

    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(entry);
    materializeLambda(b->getRhs(), m, builder, bctx);
    //builder.CreateRetVoid();

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
    for (TypeName *____ : b->types_range()) {
        // HACK:
        Elements.push_back(builder.getInt64Ty());
        // Elements.push_back(bctx.getDataTypeFromName(*Name));
    }

    StructType *Ty =
        StructType::create(m.getContext(), Elements,
                           decl->getTypeName() + "_variant_" + b->getName());
    return Ty;
};

int compile_program(stg::Program *program, int argc, char **argv) {
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

        bctx.insertTopLevelBinding(
            b, materializeTopLevelStaticBinding(b, *m, builder, bctx));
    }

    if (verifyModule(*m, nullptr) == 1) {
        cerr << "-----\n";
        cerr << "Module:\b";
        errs() << *m << "\n";
        cerr << "-----\n";
        cerr << " *** Broken module found, aborting compilation.\nError:\n";
        verifyModule(*m, &errs());
        exit(1);
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
