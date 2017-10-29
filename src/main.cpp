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
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "stgir.h"
#include "jit.h"
#include "cxxopts.hpp"
 
using namespace llvm;

// The - option defaults to opening STDOUT;
//cl::opt<std::string> OPTION_OUTPUT_FILENAME("output", cl::desc("Specify output filename"), cl::value_desc("filename"), cl::init("-"));
//cl::opt<bool> OPTION_DUMP_LLVM("emit-llvm", cl::desc("dump output in LLVM assembly, not as an object file"), cl::value_desc("write llvm to output file"), cl::init(false));


using namespace std;
using namespace stg;
using namespace llvm;

class BuildCtx;

typedef void (*SimplexhcMainTy)(void);

class StgType {
   public:
    enum StgTypeKind { STK_Data, STK_Function };
    StgTypeKind getKind() const { return kind; }
    virtual std::string getTypeName() const = 0;
    virtual void dump() const { cout << getTypeName(); }

   private:
    StgTypeKind kind;

   protected:
    StgType(StgTypeKind kind) : kind(kind){};
};

class StgDataType : public StgType {
   private:
    const DataType *datatype;

   public:
    explicit StgDataType(const DataType *datatype)
        : datatype(datatype), StgType(StgType::STK_Data){};
    const DataType *getDataType() const { return datatype; };

    std::string getTypeName() const { return datatype->getTypeName(); }

    static bool classof(const StgType *ty) { return ty->getKind() == STK_Data; }
};

class StgFunctionType : public StgType {
   private:
    const StgType *returnType;
    SmallVector<const StgType *, 4> paramTypes;

   public:
    StgFunctionType(const StgType *returnType,
                    ArrayRef<const StgType *> paramTypesref)
        : StgType(StgType::STK_Function), returnType(returnType) {
        for (const StgType *ty : paramTypesref) paramTypes.push_back(ty);
    }

    const StgType *getReturnType() const { return returnType; }

    std::string getTypeName() const {
        std::stringstream outs;
        outs << "ty-";
        returnType->dump();
        outs << "(";
        for (const StgType *t : paramTypes) {
            t->dump();
            outs << " ";
        }
        return outs.str();
    }
    static bool classof(const StgType *ty) {
        return ty->getKind() == STK_Function;
    }
};

using StgIRBuilder = IRBuilder<>;

void materializeExpr(const Expression *e, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx);

std::set<Identifier> getFreeVarsInCase(const ExpressionCase *c,
                                       const BuildCtx &bctx);

static const StgType *getTypeOfExpression(const Expression *e,
                                          const BuildCtx &bctx);

void materializeEnterInt(Value *v, std::string contName, Module &m,
                         StgIRBuilder &builder, BuildCtx &bctx);

void loadFreeVariableFromClosure(Value *closure, Identifier name,
                                 const StgType *ty, int idx,
                                 StgIRBuilder builder, BasicBlock *insertBB,
                                 BuildCtx &bctx);
struct LLVMClosureData {
    AssertingVH<Function> fn;
    AssertingVH<GlobalVariable> closure;

    LLVMClosureData(Function *fn, GlobalVariable *closure)
        : fn(fn), closure(closure){};
};

struct LLVMValueData {
    AssertingVH<Value> v;
    const StgType *stgtype;

    LLVMValueData(Value *v, const StgType *stgtype) : v(v), stgtype(stgtype) {}
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

static Value *TransmuteToInt(Value *V, StgIRBuilder &builder) {
    if (V->getType()->isIntegerTy()) return V;
    return builder.CreatePtrToInt(V, builder.getInt64Ty(),
                                  V->getName() + "_transmute_to_int");
}

// RawMem == void * (in C) == char * == i8* (in LLVM)
static Type *getRawMemTy(StgIRBuilder builder) {
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

class BuildCtx {
   public:
    using IdentifierMapTy = Scope<Identifier, LLVMValueData>;

    using StaticBindingMapTy = std::map<Identifier, LLVMClosureData>;

    // a map from data constructors to the underlying DataConstructor
    using DataConstructorMap =
        std::map<ConstructorName, std::tuple<DataConstructor *, Type *>>;

    // a map from data types to their underlying DataType.
    using TypeMapTy = std::map<TypeName, StgType *>;

    // Closure tags to global variables representing ints.
    // TODO: do this when you have more free time :)
    // using ClosureTagMap = std::map<ClosureTag, AssertingVH<GlobalVariable>>;

    AssertingVH<Function> popInt, pushInt;
    AssertingVH<Function> pushBoxed, popBoxed;
    LLVMClosureData *printInt;
    LLVMClosureData *primMultiply;
    AssertingVH<Function> malloc;
    AssertingVH<Function> pushReturnCont, popReturnCont;
    AssertingVH<GlobalVariable> stackInt;
    AssertingVH<GlobalVariable> stackIntTop;

    AssertingVH<GlobalVariable> stackBoxed;
    AssertingVH<GlobalVariable> stackBoxedTop;

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
        addStack(m, builder, getRawMemTy(builder), "Return", STACK_SIZE,
                 pushReturnCont, popReturnCont, stackReturnCont,
                 stackReturnContTop);

        // *** Heap ***
        addStack(m, builder, getRawMemTy(builder), "Boxed", STACK_SIZE,
                 pushBoxed, popBoxed, stackBoxed, stackBoxedTop);

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

        // *** enter dynamic closure ***
        enterDynamicClosure = addEnterDynamicClosureToModule(m, builder, *this);

        // *** primMultiply *** //
        primMultiply = addPrimMultiplyToModule(m, builder, *this);
        this->insertTopLevelBinding(
            "primMultiply",
            new StgFunctionType(this->primIntTy,
                                {this->primIntTy, this->primIntTy}),
            *primMultiply);

        // *** printInt *** //
        printInt = [&] {
            Function *F =
                createNewFunction(m,
                                  FunctionType::get(builder.getVoidTy(),
                                                    /*isVarArg=*/false),
                                  "printInt");

            Function *printOnlyInt = getOrCreateFunction(
                m,
                FunctionType::get(builder.getVoidTy(), {builder.getInt64Ty()},
                                  false),
                "printOnlyInt");

            BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
            builder.SetInsertPoint(entry);
            Value *i = builder.CreateCall(popInt, {}, "i");
            builder.CreateCall(printOnlyInt, {i});

            BasicBlock *exit = BasicBlock::Create(m.getContext(), "exit", F);
            builder.SetInsertPoint(exit);
            builder.CreateRetVoid();

            BasicBlock *next =
                BasicBlock::Create(m.getContext(), "callnextfn", F);
            builder.SetInsertPoint(next);
            Value *cont = builder.CreateCall(popReturnCont, {}, "next_cont");
            builder.CreateCall(enterDynamicClosure, {cont});
            builder.CreateRetVoid();

            builder.SetInsertPoint(entry);
            Value *returnTop =
                builder.CreateLoad(this->stackReturnContTop, "nReturnFrames");
            Value *haveReturnFrames = builder.CreateICmpUGT(
                returnTop, builder.getInt64(1), "haveReturnFrames");
            builder.CreateCondBr(haveReturnFrames, next, exit);

            return new LLVMClosureData(materializeStaticClosureForFn(
                F, "closure_printInt", m, builder, *this));
        }();

        this->insertTopLevelBinding(
            "printInt", new StgFunctionType(this->boxedTy, {this->primIntTy}),
            *printInt);
    }

    ~BuildCtx() {
        delete this->printInt;
        delete this->primMultiply;
        // delete this->stackReturnCont;
        // delete this->stackReturnContTop;
        // delete this->stackInt;
        // delete this->stackIntTop;
    }

    // Return the PrimInt type.
    const StgDataType *getPrimIntTy() const { return this->primIntTy; }

    // map a binding to a function in the given scope.
    void insertTopLevelBinding(std::string name, const StgType *returnTy,
                               LLVMClosureData bdata) {
        errs() << "name: " << name << "\n";
        assert(staticBindingMap.find(name) == staticBindingMap.end());
        errs() << "name: " << name << "\n\n";
        staticBindingMap.insert(std::make_pair(name, bdata));

        LLVMValueData vdata(&*bdata.closure, returnTy);
        identifiermap.insert(name, vdata);
    }

    iterator_range<StaticBindingMapTy::const_iterator> getTopLevelBindings()
        const {
        return make_range(staticBindingMap.begin(), staticBindingMap.end());
    }

    // map an identifier to a value in the current scope.
    void insertIdentifier(Identifier ident, LLVMValueData v) {
        identifiermap.insert(ident, v);
    }

    // lookup an identifier form the current scope
    LLVMValueData getIdentifier(Identifier ident) const {
        auto It = identifiermap.find(ident);
        if (It == identifiermap.end()) {
            cerr << "Unknown identifier: |" << ident << "|\n";
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

    void insertType(std::string name, StgType *datatype) {
        assert(dataTypeMap.find(name) == dataTypeMap.end() &&
               "double inserting a datatype.");
        dataTypeMap[name] = datatype;
    }

    const StgType *getTypeFromRawType(const TypeRaw *tyraw) const {
        switch (tyraw->getKind()) {
            case TypeRaw::TK_Data: {
                const DataTypeRaw *dt = cast<DataTypeRaw>(tyraw);
                return getTypeFromName(dt->getName());
            }
            case TypeRaw::TK_Function: {
                const FunctionTypeRaw *ft = cast<FunctionTypeRaw>(tyraw);
                const StgType *retty = getTypeFromName(ft->getReturnTypeName());
                SmallVector<const StgType *, 4> paramtys;
                for (TypeName n : ft->params_range()) {
                    paramtys.push_back(getTypeFromName(n));
                }
                return new StgFunctionType(retty, paramtys);
            }
        }

        report_fatal_error("should not reach here, bottom of switch");
    }
    const StgType *getTypeFromName(std::string name) const {
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
        report_fatal_error("unknown type name");
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
    TypeMapTy dataTypeMap;

    StgDataType *primIntTy;
    StgDataType *boxedTy;

    static void populateIntrinsicTypes(Module &m, StgIRBuilder &builder,
                                       TypeMapTy &typemap,
                                       DataConstructorMap &consmap,
                                       StgDataType *&primIntTy,
                                       StgDataType *&boxedTy) {
        // primInt
        DataConstructor *cons = new DataConstructor("PrimInt", {});
        consmap["PrimInt"] = std::make_pair(cons, builder.getInt64Ty());

        primIntTy = new StgDataType(new DataType("PrimInt", {cons}));
        typemap["PrimInt"] = primIntTy;

        // Boxed
        boxedTy = new StgDataType(
            new DataType("Boxed", {}));  // void has no constructors.
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
            FunctionType::get(builder.getVoidTy(), {getRawMemTy(builder)},
                              /*varargs = */ false),
            "enter_dynamic_closure");

        BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
        builder.SetInsertPoint(entry);
        Argument *closureRaw = &*F->arg_begin();
        closureRaw->setName("closure_raw");

        Value *closureStruct = builder.CreateBitCast(
            closureRaw, bctx.ClosureTy[0]->getPointerTo());
        Value *contSlot = builder.CreateGEP(
            closureStruct, {builder.getInt64(0), builder.getInt32(0)},
            "cont_slot");
        Value *cont = builder.CreateLoad(contSlot, "cont");
        // Value *contAddr = builder.CreatePtrToInt(cont,
        // builder.getInt64Ty(), "cont_addr");

        // store the address of the closure we are entering
        Value *closureAddr = builder.CreatePtrToInt(
            closureRaw, builder.getInt64Ty(), "closure_addr");
        builder.CreateStore(closureAddr, bctx.enteringClosureAddr);
        // call the function
        builder.CreateCall(cont, {});
        builder.CreateRetVoid();
        return F;
    }

    static LLVMClosureData *addPrimMultiplyToModule(Module &m,
                                                    StgIRBuilder builder,
                                                    BuildCtx &bctx) {
        Function *F =
            createNewFunction(m,
                              FunctionType::get(builder.getVoidTy(), {},
                                                /*varargs = */ false),
                              "primMultiply");
        BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
        builder.SetInsertPoint(entry);
        Value *i = builder.CreateCall(bctx.popInt, {}, "i");
        Value *j = builder.CreateCall(bctx.popInt, {}, "j");
        Value *result = builder.CreateMul(i, j, "result");

        builder.CreateCall(bctx.pushInt, {result});

        Value *RetFrame =
            builder.CreateCall(bctx.popReturnCont, {}, "return_frame");
        builder.CreateCall(bctx.enterDynamicClosure, RetFrame);
        builder.CreateRetVoid();

        return new LLVMClosureData(materializeStaticClosureForFn(
            F, "closure_primMultiply", m, builder, bctx));
    }

    LLVMClosureData createPrimFunction(
        Module &m, StgIRBuilder &builder, const BuildCtx &bctx,
        std::string fnName, StgType *functionType,
        BuildCtx::StaticBindingMapTy &staticBindingMap,
        BuildCtx::IdentifierMapTy &identifiermap) {
        Function *F = createNewFunction(m,
                                        FunctionType::get(builder.getVoidTy(),
                                                          /*isVarArg=*/false),
                                        fnName);
        LLVMClosureData data(materializeStaticClosureForFn(
            F, "closure_" + fnName, m, builder, *this));
        staticBindingMap.insert(std::make_pair(fnName, data));
        identifiermap.insert(fnName, LLVMValueData(data.closure, functionType));
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
    errs() << "V : " << *V << "\n\n";
    assert(V->getType()->isPointerTy());
    if (V->getType() != getRawMemTy(builder)) {
        V = builder.CreateBitCast(V, getRawMemTy(builder));
    }
    builder.CreateCall(bctx.enterDynamicClosure, {V});
};

// As always, the one who organises things (calls the function) does the
// work: push params in reverse order.
void materializeAp(const ExpressionAp *ap, Module &m, StgIRBuilder &builder,
                   BuildCtx &bctx) {
    for (Atom *p : ap->params_reverse_range()) {
        Value *v = materializeAtom(p, builder, bctx);
        if (isa<AtomInt>(p)) {
            builder.CreateCall(bctx.pushInt, {v});
        } else {
            LLVMValueData vdata =
                bctx.getIdentifier(cast<AtomIdent>(p)->getIdent());
            if (vdata.stgtype == bctx.getPrimIntTy()) {
                builder.CreateCall(bctx.pushInt, {v});
            } else {
                v = builder.CreateBitCast(v, getRawMemTy(builder));
                builder.CreateCall(bctx.pushBoxed, {v});
            }
        }
    }
    LLVMValueData vdata = bctx.getIdentifier(ap->getFnName());
    if (vdata.stgtype == bctx.getPrimIntTy()) {
        materializeEnterInt(vdata.v, ap->getFnName(), m, builder, bctx);
    } else {
        materializeEnterDynamicClosure(vdata.v, m, builder, bctx);
    };
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
    builder.CreateCall(bctx.pushBoxed, {rawMem});

    // now pop a continuation off the return stack and invoke it
    Value *ReturnCont =
        builder.CreateCall(bctx.popReturnCont, {}, "returncont");

    // we need to use enterDynamicClosure because we create closures for our
    // return alts as well.
    builder.CreateCall(bctx.enterDynamicClosure, ReturnCont);
};

// materialize destructure code for an alt over a constructor.
// Assumes that the builder is focused on the correct basic block.
void materializeCaseConstructorAltDestructure(const ExpressionCase *c,
                                              const CaseAltDestructure *d,
                                              Value *rawmem, Module &m,
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
    StructType *DeclTy = cast<StructType>(T);

    Value *StructPtr = builder.CreateBitCast(rawmem, DeclTy->getPointerTo(),
                                             DeclTy->getName() + "_structptr");

    // Declaration and destructuring param sizes should match.
    assert(cons->types_size() == d->variables_size());

    for (Identifier var : d->variables_range()) {
        // We need i+1 because 0th slot is used for type.
        SmallVector<Value *, 2> Idxs = {builder.getInt64(0),
                                        builder.getInt32(i + 1)};
        Value *Slot =
            builder.CreateGEP(StructPtr, Idxs, "slot_int_" + std::to_string(i));

        if (bctx.getTypeFromName(cons->getTypeName(i)) == bctx.getPrimIntTy()) {
            Value *V = builder.CreateLoad(Slot, "cons_" + std::to_string(i));
            bctx.insertIdentifier(var, LLVMValueData(V, bctx.getPrimIntTy()));
        } else {
            Value *V = builder.CreateLoad(
                Slot, "cons_mem_as_int_" + std::to_string(i));
            V = builder.CreateIntToPtr(V, getRawMemTy(builder),
                                       "cons_rawmem_" + std::to_string(i));
            const StgType *Ty = bctx.getTypeFromName(cons->getTypeName(i));
            bctx.insertIdentifier(var, LLVMValueData(V, Ty));
        }
        i++;
    }
    materializeExpr(d->getRHS(), m, builder, bctx);
}

static const StgType *getCommonDataTypeFromAlts(const ExpressionCase *c,
                                                const StgIRBuilder &builder,
                                                const BuildCtx &bctx) {
    const StgType *commondecl = nullptr;
    auto setCommonType = [&](const StgType *newdecl) -> void {
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
            setCommonType(bctx.getTypeFromName(dc->getParent()->getTypeName()));
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
Function *materializeCaseConstructorReturnFrame(
    const ExpressionCase *c, const std::vector<Identifier> freeVarsInAlts,
    Module &m, StgIRBuilder builder, BuildCtx &bctx) {
    std::stringstream scrutineeNameSS;
    scrutineeNameSS << *c->getScrutinee();
    Function *f = createNewFunction(
        m, FunctionType::get(builder.getVoidTy(), {}, /*isVarArg=*/false),
        "case_alt_" + scrutineeNameSS.str());
    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", f);
    builder.SetInsertPoint(entry);

    // **** COPY PASTE BEGIN
    // here is code duplication in some sense with
    // materializeDynamicLetBinding Open a new scope.
    BuildCtx::Scoper s(bctx);
    if (freeVarsInAlts.size() > 0) {
        Value *closureAddr =
            builder.CreateLoad(bctx.enteringClosureAddr, "closure_addr_int");
        Value *closure = builder.CreateIntToPtr(
            closureAddr, bctx.ClosureTy[freeVarsInAlts.size()]->getPointerTo(),
            "closure_typed");
        int i = 0;
        for (Identifier id : freeVarsInAlts) {
            // HACK: need correct type info
            const StgType *ty = bctx.getIdentifier(id).stgtype;
            loadFreeVariableFromClosure(closure, id, ty, i, builder, entry,
                                        bctx);
            i++;
        }
    }
    // **** COPY PASTE END
    // HACK: special case for case x of { default -> ... }
    if (c->alts_size() == 1 && isa<CaseAltDefault>(*(c->alts_begin()))) {
        assert((freeVarsInAlts.size() == 0 && "unhandled.") ||
               "checking if this works..");
        const CaseAltDefault *default_ = cast<CaseAltDefault>(*c->alts_begin());
        materializeExpr(default_->getRHS(), m, builder, bctx);
        builder.CreateRetVoid();
        return f;
    }

    Value *rawmem = builder.CreateCall(bctx.popBoxed, {}, "rawmem");

    Value *TagPtr = builder.CreateBitCast(
        rawmem, builder.getInt64Ty()->getPointerTo(), "tagptr");
    // Since we only care about the tag, we can convert to i64 and forget
    // about the rest.
    Value *Tag = builder.CreateLoad(TagPtr, "tag");

    BasicBlock *failure = BasicBlock::Create(m.getContext(), "failure", f);
    builder.SetInsertPoint(failure);
    Function *trap = getOrCreateFunction(
        m, FunctionType::get(builder.getVoidTy(), {}), "llvm.trap");
    builder.CreateCall(trap, {});
    builder.CreateRetVoid();

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
                    cast<StgDataType>(
                        getCommonDataTypeFromAlts(c, builder, bctx))
                        ->getDataType();
                const int Tag = dataType->getIndexForConstructor(std::get<0>(
                    bctx.getDataConstructorFromName(d->getConstructorName())));
                // teach the switch case to switch to this BB on
                // encountering the tag.
                switch_->addCase(builder.getInt64(Tag), bb);
                materializeCaseConstructorAltDestructure(c, d, rawmem, m,
                                                         builder, bctx);
                builder.CreateRetVoid();
                break;
            }
            case CaseAlt::CAK_Int:
                assert(false && "case of a non-int scrutinee cannot have int");
                break;
            case CaseAlt::CAK_Variable: {
                CaseAltVariable *altVariable = cast<CaseAltVariable>(a);
                BasicBlock *bb = BasicBlock::Create(m.getContext(),
                                                    altVariable->getLHS(), f);
                builder.SetInsertPoint(bb);
                // We will switch to variable case by default, since this is
                // the _variable_ case after all.
                // TODO: copy checks from generate prim int to here.
                switch_->setDefaultDest(bb);
                Value *raw = builder.CreateCall(bctx.popInt, {},
                                                altVariable->getLHS() + "_raw");
                const StgType *ty =
                    getTypeOfExpression(c->getScrutinee(), bctx);
                BuildCtx::Scoper s(bctx);
                bctx.insertIdentifier(altVariable->getLHS(),
                                      LLVMValueData(raw, ty));
                materializeExpr(altVariable->getRHS(), m, builder, bctx);
                builder.CreateRetVoid();

                break;
            }
            case CaseAlt::CAK_Default:
                assert(false && "unimplemented alt codegen for cak_default");
        }
    }
    return f;
}

// Materialize a case over Prim int.
Function *materializePrimitiveCaseReturnFrame(
    const ExpressionCase *c, const std::vector<Identifier> freeVarsInAlts,
    Module &m, StgIRBuilder builder, BuildCtx &bctx) {
    std::stringstream namess;
    namess << "case_" << *c->getScrutinee() << "_alts";

    Function *F = createNewFunction(
        m, FunctionType::get(builder.getVoidTy(), {}, /*isVarArg=*/false),
        namess.str());
    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(entry);

    // **** COPY PASTE BEGIN
    // here is code duplication in some sense with
    // materializeDynamicLetBinding Open a new scope.
    BuildCtx::Scoper s(bctx);
    if (freeVarsInAlts.size() > 0) {
        Value *closureAddr =
            builder.CreateLoad(bctx.enteringClosureAddr, "closure_addr_int");
        Value *closure = builder.CreateIntToPtr(
            closureAddr, bctx.ClosureTy[freeVarsInAlts.size()]->getPointerTo(),
            "closure_typed");
        int i = 0;
        for (Identifier id : freeVarsInAlts) {
            // HACK: need correct type info
            const StgType *ty = bctx.getIdentifier(id).stgtype;
            loadFreeVariableFromClosure(closure, id, ty, i, builder, entry,
                                        bctx);
            i++;
        }
    }
    // **** COPY PASTE END

    Value *scrutinee = builder.CreateCall(bctx.popInt, {}, "scrutinee");
    BasicBlock *defaultBB =
        BasicBlock::Create(m.getContext(), "alt_default", F);
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
                // create ret void.
                builder.SetInsertPoint(BB);
                builder.CreateRetVoid();
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
        // create ret void.
        builder.SetInsertPoint(defaultBB);

        assert(false && "unhandled default");
    } else if (const CaseAltVariable *cv = c->getVariableAlt()) {
        defaultBB->setName("var_" + cv->getLHS());
        BuildCtx::Scoper s(bctx);
        builder.SetInsertPoint(defaultBB);
        // create a binding between the scrutinee and the variable name of
        // the alt.
        bctx.insertIdentifier(cv->getLHS(),
                              LLVMValueData(scrutinee, bctx.getPrimIntTy()));
        materializeExpr(cv->getRHS(), m, builder, bctx);
        // create ret void.
        builder.CreateRetVoid();

    } else {
        builder.SetInsertPoint(defaultBB);
        Function *trap = getOrCreateFunction(
            m, FunctionType::get(builder.getVoidTy(), {}), "llvm.trap");
        builder.CreateCall(trap, {});
    }
    return F;
}

static const StgType *getTypeOfExpression(const Expression *e,
                                          const BuildCtx &bctx) {
    switch (e->getKind()) {
        case Expression::EK_Ap: {
            const ExpressionAp *ap = cast<ExpressionAp>(e);
            const std::string fnName = ap->getFnName();
            const StgFunctionType *Fty =
                cast<StgFunctionType>(bctx.getIdentifier(fnName).stgtype);
            return Fty->getReturnType();
            break;
        }
        case Expression::EK_IntLiteral: {
            return bctx.getPrimIntTy();
            break;
        }
        case Expression::EK_Case:
        case Expression::EK_Cons:
        case Expression::EK_Let:
            report_fatal_error("foo");
            assert(false && "unimplemented getTypeOfExpression");
    }
    assert(false && "unreachable");
}

std::set<Identifier> getIdentifiersInAltLHS(const CaseAlt *alt);

// Get all identifiers referred to by this expression, whether free or
// bound.
std::set<Identifier> getFreeVarsInExpression(
    const Expression *e,
    iterator_range<BuildCtx::StaticBindingMapTy::const_iterator>
        staticBindingsRange) {
    auto extractIdentifierFromAtom = [](std::set<Identifier> &s,
                                        const Atom *a) {
        if (const AtomIdent *id = dyn_cast<AtomIdent>(a))
            s.insert(id->getIdent());
    };

    std::set<Identifier> freeVars;

    switch (e->getKind()) {
        case Expression::EK_IntLiteral:
            break;
        case Expression::EK_Ap: {
            const ExpressionAp *ap = cast<ExpressionAp>(e);
            freeVars.insert(ap->getFnName());
            for (const Atom *p : ap->params_range())
                extractIdentifierFromAtom(freeVars, p);
            break;
        }
        case Expression::EK_Cons: {
            const ExpressionConstructor *cons = cast<ExpressionConstructor>(e);
            for (const Atom *a : cons->args_range())
                extractIdentifierFromAtom(freeVars, a);
            break;
        }
        case Expression::EK_Let:
            assert(false && "unimplemented");
        case Expression::EK_Case: {
            const ExpressionCase *c = cast<ExpressionCase>(e);
            // HACK: do not consider scrutinee as free. This is a HACK -_-"
            // cerr << "HACK: right now, we do not consider the scrutinee as
            // "
            //        "free. This is a hack because I have more interesting
            //        " "things I want to try\n";
            // Add the free vars in the scrutinee into the freeVars list.
            const std::set<Identifier> scrutineeFree(getFreeVarsInExpression(
                c->getScrutinee(), staticBindingsRange));
            freeVars.insert(scrutineeFree.begin(), scrutineeFree.end());

            for (const CaseAlt *alt : c->alts_range()) {
                // The LHS of a case alt contains bindings that are bound
                // throughout the alt subexpression. So, we can subtract
                // those from the RHS.
                const std::set<Identifier> lhsBound =
                    getIdentifiersInAltLHS(alt);
                const std::set<Identifier> rhsFree =
                    getFreeVarsInExpression(alt->getRHS(), staticBindingsRange);

                std::set_difference(rhsFree.begin(), rhsFree.end(),
                                    lhsBound.begin(), lhsBound.end(),
                                    std::inserter(freeVars, freeVars.begin()));
            }
            break;
        }
    };

    // We can remove top-level bindings from this list, because they are
    // always available, so they are "pseudo bound".
    for (auto It : staticBindingsRange) {
        freeVars.erase(It.first);
    }
    return freeVars;
}

// Get the names of the variables that are feshly bound by an alt.
// The names that are bound by an alt come from the LHS of the alt.
std::set<Identifier> getIdentifiersInAltLHS(const CaseAlt *alt) {
    switch (alt->getKind()) {
        case CaseAlt::CAK_Default:
            return {};
        case CaseAlt::CAK_Variable: {
            const CaseAltVariable *var = cast<CaseAltVariable>(alt);
            return {var->getLHS()};
        }
        case CaseAlt::CAK_Int:
            return {};
        case CaseAlt::CAK_Destructure: {
            std::set<Identifier> ids;
            const CaseAltDestructure *destructure =
                cast<CaseAltDestructure>(alt);
            for (Identifier id : destructure->variables_range()) ids.insert(id);
            return ids;
        }
    }
    assert(false && "unreachable");
}

void materializeCase(const ExpressionCase *c, Module &m, StgIRBuilder &builder,
                     BuildCtx &bctx) {
    // use std::vector to prevent all kinds of subtle bugs with reordering.
    const std::vector<Identifier> freeVarsInAlts = [&]() {
        // Collect all free variables in the alternates.
        std::set<Identifier> ids =
            getFreeVarsInExpression(c, bctx.getTopLevelBindings());

        std::vector<Identifier> vecids;
        for (Identifier id : ids) {
            vecids.push_back(id);
        }
        return vecids;
    }();

    const Expression *scrutinee = c->getScrutinee();
    const StgType *scrutineety = getTypeOfExpression(scrutinee, bctx);

    Function *continuation = [&] {
        if (bctx.getPrimIntTy() == scrutineety) {
            return materializePrimitiveCaseReturnFrame(c, freeVarsInAlts, m,
                                                       builder, bctx);
        } else {
            return materializeCaseConstructorReturnFrame(c, freeVarsInAlts, m,
                                                         builder, bctx);
        }
    }();

    Value *clsRaw =
        builder.CreateCall(bctx.malloc,
                           {builder.getInt64(m.getDataLayout().getTypeAllocSize(
                               bctx.ClosureTy[freeVarsInAlts.size()]))},
                           "closure_raw");
    Value *clsTyped = builder.CreateBitCast(
        clsRaw, bctx.ClosureTy[freeVarsInAlts.size()]->getPointerTo(),
        "closure_typed");
    Value *fnSlot = builder.CreateGEP(
        clsTyped, {builder.getInt64(0), builder.getInt32(0)}, "fn_slot");
    builder.CreateStore(continuation, fnSlot);

    // store free vars into the slot.
    int i = 0;
    for (Identifier freeVar : freeVarsInAlts) {
        Value *freeVarSlot = builder.CreateGEP(
            clsTyped,
            {builder.getInt64(0), builder.getInt32(1), builder.getInt32(i)},
            freeVar + "_free_in_case_slot");
        Value *freeVarVal = bctx.getIdentifier(freeVar).v;
        freeVarVal = TransmuteToInt(freeVarVal, builder);
        builder.CreateStore(freeVarVal, freeVarSlot);
        i++;
    }

    builder.CreateCall(bctx.pushReturnCont, {clsRaw});
    materializeExpr(scrutinee, m, builder, bctx);

    // clean this up, I should need this epilogue for materializeExpr in the
    // other case as well :(. if(bctx.isPrimIntTy(scrutineety)) {
    //     builder.CreateRetVoid();
    // }
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

// Load free parameters from the closure `closure`, with free parameters
// from `paramsRange` at basic block `insertBB`. This is useful since
// closures are shared by both case for alternatives that need to capture
// free variables, as well as by regular lambdas that need free variables.
void loadFreeVariableFromClosure(Value *closure, Identifier name,
                                 const StgType *ty, int idx,
                                 StgIRBuilder builder, BasicBlock *insertBB,
                                 BuildCtx &bctx) {
    builder.SetInsertPoint(insertBB);
    Value *v = builder.CreateGEP(
        closure,
        {builder.getInt64(0), builder.getInt32(1), builder.getInt32(idx)},
        "free__" + name + "__ty_" + ty->getTypeName());

    v = builder.CreateLoad(v, name);
    if (ty != bctx.getPrimIntTy()) {
        v = builder.CreateIntToPtr(v, getRawMemTy(builder), name + "_rawmem");
    }

    bctx.insertIdentifier(name, LLVMValueData(v, ty));
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
    BuildCtx::Scoper s(bctx);

    int i = 0;
    for (Parameter *p : b->getRhs()->free_params_range()) {
        const StgType *ty = bctx.getTypeFromRawType(p->getTypeRaw());
        loadFreeVariableFromClosure(closure, p->getName(), ty, i, builder,
                                    entry, bctx);
        i++;
    }
    materializeLambda(b->getRhs(), m, builder, bctx);
    builder.CreateRetVoid();
    return F;
}

StgFunctionType *createStgTypeForLambda(const Lambda *l, const BuildCtx &bctx) {
    SmallVector<const StgType *, 4> paramsty;
    for (Parameter *p : l->bound_params_range()) {
        paramsty.push_back(bctx.getTypeFromRawType(p->getTypeRaw()));
    }
    const StgType *retty = bctx.getTypeFromName(l->getReturnTypeName());
    return new StgFunctionType(retty, paramsty);
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

        const StgType *bindingty = createStgTypeForLambda(b->getRhs(), bctx);
        // So, because function applications are _fully saturated_, I can
        // consider the type of a let-binding to be the return type. Is this
        // cheating? Sure.
        assert(true && "what type do I assign to a let-binding?");
        bctx.insertIdentifier(b->getName(), LLVMValueData(cls, bindingty));
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

// Rule to enter an int: pop a return continuation and evaluate it with this
// int.
void materializeEnterInt(Value *v, std::string contName, Module &m,
                         StgIRBuilder &builder, BuildCtx &bctx) {
    builder.CreateCall(bctx.pushInt, {v});
    Value *cont =
        builder.CreateCall(bctx.popReturnCont, {}, "return_cont_" + contName);
    materializeEnterDynamicClosure(cont, m, builder, bctx);
}

// rule to enter an int literal is to simply enter an int.
void materializeExprIntLiteral(const ExpressionIntLiteral *e, Module &m,
                               StgIRBuilder &builder, BuildCtx &bctx) {
    materializeEnterInt(builder.getInt64(e->getValue()),
                        std::to_string(e->getValue()), m, builder, bctx);
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
        const StgType *Ty = bctx.getTypeFromRawType(p->getTypeRaw());
        if (Ty == bctx.getPrimIntTy()) {
            Value *pv =
                builder.CreateCall(bctx.popInt, {}, "param_" + p->getName());
            bctx.insertIdentifier(p->getName(),
                                  LLVMValueData(pv, bctx.getPrimIntTy()));
        } else {
            Value *pv =
                builder.CreateCall(bctx.popBoxed, {}, "param_" + p->getName());
            bctx.insertIdentifier(p->getName(), LLVMValueData(pv, Ty));
        }
    }
    materializeExpr(l->getRhs(), m, builder, bctx);
}

// Create a static closure for a top-level function. That way, we don't need
// to repeatedly build the closure.
LLVMClosureData materializeStaticClosureForFn(Function *F, std::string name,
                                              Module &m, StgIRBuilder &builder,
                                              BuildCtx &bctx) {
    StructType *closureTy = bctx.ClosureTy[0];

    // 2. Create the initializer for the closure
    Constant *initializer = [&] {
        return ConstantStruct::get(closureTy, {F});
    }();

    GlobalVariable *closure =
        new GlobalVariable(m, closureTy, /*isconstant=*/true,
                           GlobalValue::ExternalLinkage, initializer, name);
    return LLVMClosureData(F, closure);
}

// Create a top-level static binding from a "binding" that is parsed in STG.
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
    for (TypeName *ty : b->types_range()) {
        errs() << "type: " << *ty << "\n";
        // HACK:
        Elements.push_back(builder.getInt64Ty());
        // Elements.push_back(bctx.getTypeFromName(*Name));
    }

    StructType *Ty =
        StructType::create(m.getContext(), Elements,
                           decl->getTypeName() + "_variant_" + b->getName());

    errs() << "StructType: " << *Ty << "\n";
    return Ty;
};


int compile_program(stg::Program *program, cxxopts::Options &opts) {

    // ask LLVM to kindly initialize all of its knowledge about targets.
    InitializeAllTargetInfos();
    InitializeAllTargets();
    InitializeAllTargetMCs();
    InitializeAllAsmParsers();
    InitializeAllAsmPrinters();

    const std::string OPTION_OUTPUT_FILENAME = opts["o"].as<std::string>();
    const bool OPTION_DUMP_LLVM = opts.count("emit-llvm") > 0;
    const bool OPTION_JIT = opts.count("jit") > 0;

    static LLVMContext ctx;
    static StgIRBuilder builder(ctx);

    std::unique_ptr<Module> m(new Module("Module", ctx));

    cerr << "----\n";
    cerr << "Source program: ";
    cerr << *program << "\n";
    cerr << "----\n";

    BuildCtx bctx(*m, builder);
    Binding *entrystg = nullptr;
    for (DataType *datatype : program->datatypes_range()) {
        assert(datatype->constructors_size() > 0);
        bctx.insertType(datatype->getTypeName(), new StgDataType(datatype));
        for (DataConstructor *cons : datatype->constructors_range()) {
            bctx.insertDataConstructor(
                    cons->getName(), cons,
                    materializeDataConstructor(datatype, cons, *m, builder,
                                               bctx));
        }
    }

    for (Binding *b : program->bindings_range()) {
        if (b->getName() == "main") {
            assert(!entrystg && "program has more than one main.");
            entrystg = b;
        }

        bctx.insertTopLevelBinding(
                b->getName(), createStgTypeForLambda(b->getRhs(), bctx),
                materializeTopLevelStaticBinding(b, *m, builder, bctx));
    }

    if (verifyModule(*m, nullptr) == 1) {
        cerr << "-----\n";
        cerr << "Module:\n";
        errs() << *m << "\n";
        cerr << "-----\n";
        cerr << " *** Broken module found, aborting compilation.\nError:\n";
        verifyModule(*m, &errs());
        exit(1);
    }

    std::error_code errcode;
    llvm::raw_fd_ostream outputFile(OPTION_OUTPUT_FILENAME, errcode,
                                    llvm::sys::fs::F_None);

    if (errcode) {
        std::cerr << "Unable to open output file: " << OPTION_OUTPUT_FILENAME << "\n";
        std::cerr << "Error: " << errcode.message() << "\n";
        exit(1);
    }

    if (OPTION_DUMP_LLVM) {
        m->print(outputFile, nullptr);
    }
    else {
        if (OPTION_OUTPUT_FILENAME == "-"){
            errs() << "WARNING: trying to print object file to stdout, this will be ugly. skipping because this is pointless for now.\n";
        }
        else {
            const std::string CPU = "generic";
            auto TargetTriple = sys::getDefaultTargetTriple();
            std::string Error;
            auto Target = TargetRegistry::lookupTarget(TargetTriple, Error);
            if (!Target) {
                errs() << Error;
                report_fatal_error("unable to lookup target");
            }


            TargetOptions opt;
            auto RM = Optional<Reloc::Model>();
            const std::string Features = "";
            llvm::TargetMachine *TM = Target->createTargetMachine(TargetTriple,
                                                                  CPU, Features,
                                                                  opt, RM);
            m->setDataLayout(TM->createDataLayout());
            auto FileType = TargetMachine::CGFT_ObjectFile;

            legacy::PassManager PM;
            if (TM->addPassesToEmitFile(PM, outputFile, FileType)) {
                report_fatal_error(
                        "Target machine can't emit a file of this type.");
            }
            PM.run(*m);
        }
    }
    outputFile.flush();


    if(OPTION_JIT){
        errs() << "---------\n";
        errs() << "JIT: executing module:\n";
        SimpleJIT jit;
        SimpleJIT::ModuleHandle H = jit.addModule(CloneModule(m.get()));
        Expected<JITTargetAddress> maybeMain = jit.findSymbol("main").getAddress();
        if (!maybeMain) {
            errs() << "unable to find `main` in given module:\n";
            m->print(outs(), nullptr);
            exit(1);
        }
        SimplexhcMainTy main = (SimplexhcMainTy) maybeMain.get();
        std::cout.flush();
        main();
        std::cout.flush();
        outs() << "\n----------------\n";
    }

    return 0;
}
