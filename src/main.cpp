#include <iostream>
#include <set>
#include <sstream>
#include "cxxopts.hpp"
#include "jit.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/ValueHandle.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/IPO/AlwaysInliner.h"
#include "llvm/Transforms/Utils/Cloning.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "sxhc/RuntimeDebugBuilder.h"
#include "sxhc/libstg.h"
#include "sxhc/optimizer.h"
#include "sxhc/stgir.h"

using namespace llvm;

// The - option defaults to opening STDOUT;
// cl::opt<std::string> OPTION_OUTPUT_FILENAME("output", cl::desc("Specify
// output filename"), cl::value_desc("filename"), cl::init("-"));  cl::opt<bool>
// OPTION_DUMP_LLVM("emit-llvm", cl::desc("dump output in LLVM assembly, not as
// an object file"), cl::value_desc("write llvm to output file"),
// cl::init(false));

using namespace std;
using namespace stg;
using namespace llvm;

class BuildCtx;
class AliasCtx;

typedef void (*SimplexhcMainTy)(void);
typedef void (*SimplexhcInitRawmemConstructorTy)(void);

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
        : StgType(StgType::STK_Data), datatype(datatype){};
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
        outs << "fnty-";
        outs << returnType->getTypeName();
        outs << "(";
        for (const StgType *t : paramTypes) {
            t->dump();
            outs << " ";
        }
        outs <<")";
        return outs.str();
    }
    static bool classof(const StgType *ty) {
        return ty->getKind() == STK_Function;
    }
};

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

void materializeEnterDynamicClosure(Value *V, Module &m, StgIRBuilder &builder,
                                    BuildCtx &bctx);

// Set AA metadata if the value is an instruction.
void setValueAAMetadata(Value *V, const AAMDNodes &N) {
    if (Instruction *I = dyn_cast<Instruction>(V)) I->setAAMetadata(N);
}

class LLVMClosureData {
   public:
       using FreeVarsTy = std::vector<Value *>;
       using iterator = FreeVarsTy::iterator;

    LLVMClosureData(Function *dynamicCallFn, Function *staticCallFn,
                    Value *closure, const std::vector<Value *> &freeVars)
        : dynamicCallFn(dynamicCallFn),
          staticCallFn(staticCallFn),
          closure(closure), freeVars(freeVars.begin(), freeVars.end()) {
          };
    Function *getDynamicCallFn() { return dynamicCallFn; }
    Function *getStaticCallFn() { return staticCallFn; }
    Value *getClosure() { return closure; }

    unsigned size() const { return freeVars.size(); }
    iterator free_begin() { return freeVars.begin(); }
    iterator free_end() { return freeVars.end(); }
    iterator_range<iterator> free_vars() { return make_range(free_begin(), free_end()); }

   private:
    Function *dynamicCallFn;
    Function *staticCallFn;
    Value *closure;
    FreeVarsTy freeVars;
};

struct LLVMValueData {
    Value* v;
    const StgType *stgtype;

    LLVMValueData(Value *v, const StgType *stgtype) : v(v), stgtype(stgtype) {}
};


LLVMClosureData materializeEmptyTopLevelStaticBinding(const Binding *b,
                                                      Module &m,
                                                      StgIRBuilder &builder,
                                                      BuildCtx &bctx);

void materializeLambdaDynamic(const Lambda *l, Module &m, StgIRBuilder &builder,
                              BuildCtx &bctx);

void materializeLambdaStatic(const Lambda *l, Function *F, Module &m,
                             StgIRBuilder builder, BuildCtx &bctx);

LLVMClosureData materializeStaticClosure(Function *Dynamic, Function *Static, std::string name,
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
    return Function::Create(FTy, GlobalValue::ExternalLinkage, name, &m);
}

static void createCallTrap(Module &m, StgIRBuilder &builder) {
    Function *trap = getOrCreateFunction(
        m, FunctionType::get(builder.getVoidTy(), {}), "llvm.trap");
    builder.CreateCall(trap);
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
            errs() << "insert " << k << " => " << *v.v << "\n";
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

static const int STACK_SIZE = 1000ull * 1000ull;

class AliasCtx {
    MDNode *caseClosureScopeList;
    MDNode *caseClosureScopeDomain;
    MDNode *caseClosureScope;
    AAMDNodes *caseClosureScopeAAMD;

   public:
    AliasCtx(Module &m) {
        MDBuilder builder(m.getContext());
        caseClosureScopeDomain =
            builder.createAliasScopeDomain("caseClosureScopeDomain");
        caseClosureScope = builder.createAliasScope("caseClosureScope",
                                                    caseClosureScopeDomain);
        caseClosureScopeList = MDTuple::get(m.getContext(), {caseClosureScope});
        caseClosureScopeAAMD =
            new AAMDNodes(nullptr, caseClosureScopeList, nullptr);
    }

    const AAMDNodes &getCaseClosureScopeAAMD() const {
        return *caseClosureScopeAAMD;
    }
};

class BuildCtx {
   public:
    AliasCtx aliasctx;

    using IdentifierMapTy = Scope<Identifier, LLVMValueData>;

    using StaticBindingMapTy = std::map<Identifier, LLVMClosureData>;

    // a map from data constructors to the underlying DataConstructor
    using DataConstructorMap =
        std::map<ConstructorName, std::pair<DataConstructor *, Type *>>;

    // a map from data types to their underlying DataType.
    using TypeMapTy = std::map<TypeName, StgType *>;

    // Closure tags to global variables representing ints.
    // TODO: do this when you have more free time :)
    // using ClosureTagMap = std::map<ClosureTag, AssertingVH<GlobalVariable>>;

    LLVMClosureData *printInt;
    LLVMClosureData *primMultiply;
    GlobalVariable* stackInt;
    GlobalVariable* stackIntTop;

    GlobalVariable* stackBoxed;
    GlobalVariable* stackBoxedTop;

    // AssertingVH<GlobalVariable> enteringClosureAddr;
    Function* enterDynamicClosure;

    static const unsigned MAX_FREE_PARAMS = 10;
    // type of closure { i64 tag, () -> () fn, <free vars> }
    StructType *ClosureTy[MAX_FREE_PARAMS];
    // type of values in return stack: () -> ()
    Type *ContTy;
    // stack of return values, in reality a large array
    GlobalVariable *stackReturnCont;
    // pointer to offset to top of return stack.
    GlobalVariable *stackReturnContTop;

    GlobalVariable *llvmNoDebug;
    bool nodebug;

    BuildCtx(Module &m, StgIRBuilder &builder, bool nodebug)
        : aliasctx(m), nodebug(nodebug) {
        // errs() << "HACK: setting nodebug to false.\n";
        // nodebug = false;

        llvmNoDebug = new GlobalVariable(
            m, builder.getInt1Ty(), /*isConstant=*/true,
            GlobalValue::ExternalLinkage, builder.getInt1(nodebug), "nodebug");

        MDBuilder mdbuilder(m.getContext());
        invariantGroupNode = mdbuilder.createAnonymousAliasScopeDomain(
            "closure_invariant_group");

        // *** ContTy ***
        ContTy = FunctionType::get(builder.getVoidTy(),
                                   {builder.getInt8PtrTy()}, false);

        populateIntrinsicTypes(m, builder, dataTypeMap, dataConstructorMap,
                               primIntTy, boxedTy);

        realMalloc = createRealMalloc(m, builder, *this);
        bumpPointerAlloc =
            createBumpPointerAllocator(m, builder, *this, realMalloc);
        // *** Int ***
        addStack(m, builder, *this, builder.getInt64Ty(), "Int", STACK_SIZE,
                 pushInt, popInt, stackInt, stackIntTop, llvmNoDebug);

        pushInt->setCallingConv(CallingConv::Fast);
        popInt->setCallingConv(CallingConv::Fast);

        // type of returns.
        addStack(m, builder, *this, getRawMemTy(builder), "Return", STACK_SIZE,
                 pushReturnCont, popReturnCont, stackReturnCont,
                 stackReturnContTop, llvmNoDebug);

        pushReturnCont->setCallingConv(CallingConv::Fast);
        popReturnCont->setCallingConv(CallingConv::Fast);

        // *** Heap ***
        addStack(m, builder, *this, getRawMemTy(builder), "Boxed", STACK_SIZE,
                 pushBoxed, popBoxed, stackBoxed, stackBoxedTop, llvmNoDebug);

        pushBoxed->setCallingConv(CallingConv::Fast);
        popBoxed->setCallingConv(CallingConv::Fast);

        // *** enteringClosureAddr ***
        // enteringClosureAddr = new GlobalVariable(
        //     m, builder.getInt64Ty(), /*isConstant=*/false,
        //     GlobalValue::ExternalLinkage,
        //     ConstantInt::get(builder.getInt64Ty(), 0),
        //     "enteringClosureAddr");

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
        primMultiply = addPrimArithBinopToModule(
            "primMultiply",
            [](StgIRBuilder builder, Value *a, Value *b) {
                return builder.CreateMul(a, b);
            },
            m, builder, *this);

        this->insertTopLevelBinding(
            "primMultiply",
            new StgFunctionType(this->primIntTy,
                                {this->primIntTy, this->primIntTy}),
            *primMultiply);

        // *** primSubtract *** //
        LLVMClosureData *primSubtract = addPrimArithBinopToModule(
            "primSubtract",
            [](StgIRBuilder builder, Value *a, Value *b) {
                return builder.CreateSub(a, b);
            },
            m, builder, *this);
        this->insertTopLevelBinding(
            "primSubtract",
            new StgFunctionType(this->primIntTy,
                                {this->primIntTy, this->primIntTy}),
            *primSubtract);
        delete primSubtract;

        // *** primAdd *** //
        LLVMClosureData *primAdd = addPrimArithBinopToModule(
            "primAdd",
            [](StgIRBuilder builder, Value *a, Value *b) {
                return builder.CreateAdd(a, b);
            },
            m, builder, *this);
        this->insertTopLevelBinding(
            "primAdd",
            new StgFunctionType(this->primIntTy,
                                {this->primIntTy, this->primIntTy}),
            *primAdd);
        delete primAdd;

        // *** primMult *** //
        //
        LLVMClosureData *primMult = addPrimArithBinopToModule(
            "primMult",
            [](StgIRBuilder builder, Value *a, Value *b) {
                return builder.CreateMul(a, b);
            },
            m, builder, *this);
        this->insertTopLevelBinding(
            "primMult",
            new StgFunctionType(this->primIntTy,
                                {this->primIntTy, this->primIntTy}),
            *primMult);
        delete primMult;

        // *** printInt *** //
        printInt = [&] {

            Function *printOnlyInt = getOrCreateFunction(
                m,
                FunctionType::get(builder.getVoidTy(), {builder.getInt64Ty()},
                                  false),
                "printOnlyInt");


            auto createPrint = [this](Module &m, Function *F, Value *Int, StgIRBuilder builder, Function *printOnlyInt) {
                // BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
                // builder.SetInsertPoint(entry);
                // Value *i = this->createPopInt(builder, "i");
                builder.SetInsertPoint(&F->getEntryBlock());
                builder.CreateCall(printOnlyInt, {Int});

                BasicBlock *exit = BasicBlock::Create(m.getContext(), "exit", F);
                builder.SetInsertPoint(exit);
                builder.CreateRetVoid();

                BasicBlock *next =
                    BasicBlock::Create(m.getContext(), "callnextfn", F);
                builder.SetInsertPoint(next);
                Value *cont = this->createPopReturn(builder, "next_cont");

                materializeEnterDynamicClosure(cont, m, builder, *this);
                builder.CreateRetVoid();

                builder.SetInsertPoint(&F->getEntryBlock());
                LoadInst *returnTop =
                    builder.CreateLoad(this->stackReturnContTop, "nReturnFrames");
                returnTop->setMetadata(LLVMContext::MD_invariant_group,
                        this->getInvariantGroupNode());

                Value *haveReturnFrames = builder.CreateICmpUGT(
                        returnTop, builder.getInt64(1), "haveReturnFrames");
                builder.CreateCondBr(haveReturnFrames, next, exit);
            };

            Function *Dynamic = [&] {
                Function *F = createNewFunction(
                        m,
                        FunctionType::get(builder.getVoidTy(), {builder.getInt8PtrTy()},
                            /*isVarArg=*/false),
                        "printIntDynamic");
                F->setCallingConv(CallingConv::Fast);
                F->setLinkage(GlobalValue::InternalLinkage);
                BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
                builder.SetInsertPoint(entry);
                Value *i = this->createPopInt(builder, "i");
                createPrint(m, F, i, builder, printOnlyInt);
                return F;
            }();


            Function *Static = [&] {
                Function *F = createNewFunction(
                        m,
                        FunctionType::get(builder.getVoidTy(), {builder.getInt64Ty(), builder.getInt8PtrTy()},
                            /*isVarArg=*/false),
                        "printIntStatic");
                F->setCallingConv(CallingConv::Fast);
                F->setLinkage(GlobalValue::InternalLinkage);
                BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
                builder.SetInsertPoint(entry);
                Value *i = F->arg_begin();
                createPrint(m, F, i, builder, printOnlyInt);
                return F;
            }();

            return new LLVMClosureData(materializeStaticClosure(
                Dynamic, Static, "closure_printInt", m, builder, *this));
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

    void createPushInt(StgIRBuilder &builder, Value *Int) const {
        CallInst *CI = builder.CreateCall(this->pushInt, {Int});
        CI->setCallingConv(CallingConv::Fast);
    };

    Value *createPopInt(StgIRBuilder &builder, std::string name) const {
        CallInst *CI = builder.CreateCall(this->popInt, {}, name);
        CI->setCallingConv(CallingConv::Fast);
        return CI;
    }

    void createPushBoxed(StgIRBuilder &builder, Value *Boxed) const {
        Value *BoxedVoidPtr =
            builder.CreateBitCast(Boxed, builder.getInt8Ty()->getPointerTo(),
                                  Boxed->getName() + ".voidptr");
        CallInst *CI = builder.CreateCall(this->pushBoxed, {BoxedVoidPtr});
        CI->setCallingConv(CallingConv::Fast);
    };

    Value *createPopBoxedVoidPtr(StgIRBuilder &builder,
                                 std::string name) const {
        CallInst *CI =
            builder.CreateCall(this->popBoxed, {}, name + ".voidptr");
        CI->setCallingConv(CallingConv::Fast);
        return CI;
        // return CI;
    }

    Value *createPushReturn(StgIRBuilder &builder, Value *Cont) const {
        Value *voidptr =
            builder.CreateBitCast(Cont, builder.getInt8Ty()->getPointerTo(),
                                  Cont->getName() + ".voidptr");
        CallInst *CI = builder.CreateCall(this->pushReturnCont, {voidptr});
        CI->setCallingConv(CallingConv::Fast);
        return CI;
    }

    CallInst *createPopReturn(StgIRBuilder &builder, std::string name) const {
        CallInst *CI = builder.CreateCall(this->popReturnCont, {}, name);
        CI->setCallingConv(CallingConv::Fast);
        return CI;
    }

    // Return the PrimInt type.
    const StgDataType *getPrimIntTy() const { return this->primIntTy; }

    // map a binding to a function in the given scope.
    void insertTopLevelBinding(std::string name, const StgType *returnTy,
                               LLVMClosureData bdata) {
        assert(staticBindingMap.find(name) == staticBindingMap.end());
        staticBindingMap.insert(std::make_pair(name, bdata));

        LLVMValueData vdata(&*bdata.getClosure(), returnTy);
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

        errs() << "Identifier(" << ident << ") -> " << *It->second.v << "\n";
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

    MDNode *getInvariantGroupNode() {
        assert(invariantGroupNode);
        return invariantGroupNode;
    }

    Function *getRealMalloc() const { return realMalloc; }

    void setRealMalloc(Function *newRealMalloc) {
        assert(newRealMalloc != nullptr);
        assert(this->getRealMalloc()->getNumUses() == 0);
        realMalloc = newRealMalloc;
    }

    Function *getBumpPointerAllocator() const { return bumpPointerAlloc; }

    Value *createCallAllocate(StgIRBuilder &builder, uint64_t bytes,
                              std::string name, Type *resultPointerTy) {
        CallInst *rawmem = builder.CreateCall(
            this->bumpPointerAlloc, {builder.getInt64(bytes)}, name + ".raw");
        rawmem->addAttribute(AttributeList::ReturnIndex,
                             llvm::Attribute::NoAlias);
        rawmem->setCallingConv(CallingConv::Fast);
        assert(rawmem->returnDoesNotAlias());

        if (resultPointerTy) {
            Value *typed =
                builder.CreateBitCast(rawmem, resultPointerTy, name + ".typed");

            return typed;
        } else {
            return rawmem;
        }
    }

    // Class to create and destroy a scope with RAII.
    class Scoper {
       public:
        Scoper(BuildCtx &bctx) : bctx(bctx) { bctx.pushScope(); };

        ~Scoper() { bctx.popScope(); }

       private:
        BuildCtx &bctx;
    };

    // NOTE: this is public because we need this for our hack :]
    static Function *createRealMalloc(Module &m, StgIRBuilder builder,
                                      BuildCtx &bctx) {
        return createNewFunction(
            m,
            FunctionType::get(builder.getInt8Ty()->getPointerTo(),
                              {builder.getInt64Ty()}, false),
            "malloc");
    }

   private:
    Function *pushInt, *popInt;
    Function *pushBoxed, *popBoxed;
    Function *pushReturnCont, *popReturnCont;
    Function *bumpPointerAlloc;
    Function *realMalloc;
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

    MDNode *invariantGroupNode = nullptr;

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

    static void addStack(Module &m, StgIRBuilder &builder, BuildCtx &bctx,
                         Type *elemTy, std::string name, size_t size,
                         Function *&pushFn,
                         Function *&popFn,
                         GlobalVariable *&stack,
                         GlobalVariable *&stackTop,
                         GlobalVariable *llvmNoDebug) {
        popFn = createNewFunction(
            m, FunctionType::get(elemTy, /*isVarArg=*/false), "pop" + name);
        pushFn =
            createNewFunction(m,
                              FunctionType::get(builder.getVoidTy(), {elemTy},
                                                /*isVarArg=*/false),
                              "push" + name);

        // Disable inlining of push/pop so that we can inspect it.
        pushFn->addFnAttr(llvm::Attribute::NoInline);
        popFn->addFnAttr(llvm::Attribute::NoInline);

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

        addPushToModule(m, builder, bctx, size, pushFn, stackTop, stack,
                        llvmNoDebug);
        addPopToModule(m, builder, popFn, stackTop, stack, llvmNoDebug);
    }

    static void addPushToModule(Module &m, StgIRBuilder &builder,
                                BuildCtx &bctx, size_t size, Function *F,
                                Value *stackTop, Value *stack,
                                Value *llvmNoDebug) {
        assert(F);
        assert(stackTop);
        assert(stack);

        F->addFnAttr(llvm::Attribute::InaccessibleMemOnly);
        // If the stack is a stack of pointer things, then we should tag the
        // parameter with a readonly.
        if (stackTop->getType()->getPointerElementType()->isPointerTy())
            F->addParamAttr(0, llvm::Attribute::ReadOnly);

        BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
        builder.SetInsertPoint(entry);
        // pushInt has only one argument
        Argument *arg(F->arg_begin());

        arg->setName("i");
        Value *idx = builder.CreateLoad(stackTop, "idx");

        // success
        BasicBlock *success = BasicBlock::Create(m.getContext(), "success", F);
        builder.SetInsertPoint(success);
        Value *stackSlot =
            builder.CreateGEP(stack, {builder.getInt64(0), idx}, "slot");
        StoreInst *SI = builder.CreateStore(arg, stackSlot);
        SI->setMetadata(LLVMContext::MD_invariant_group,
                        bctx.getInvariantGroupNode());

        Value *idxInc = builder.CreateAdd(idx, builder.getInt64(1), "idx_inc");
        builder.CreateStore(idxInc, stackTop);
        builder.CreateRetVoid();

        // failure--
        BasicBlock *failure = BasicBlock::Create(m.getContext(), "failure", F);
        builder.SetInsertPoint(failure);
        sxhc::RuntimeDebugBuilder::createCPUPrinter(
            builder, "Ran out of stack: ", F->getName(),
            "| size:", std::to_string(size), "|cur: ", idx, "\n");
        Function *trap = getOrCreateFunction(
            m, FunctionType::get(builder.getVoidTy(), {}), "llvm.trap");
        builder.CreateCall(trap);
        builder.CreateRetVoid();

        // entry--
        static const int SAFETY = 5;
        builder.SetInsertPoint(entry);
        Value *isInbounds = builder.CreateICmpULE(
            idx, builder.getInt64(size - 1 - SAFETY), "is_idx_inbounds");
        Value *nodebug = builder.CreateLoad(llvmNoDebug, "nodebug");

        isInbounds = builder.CreateOr(isInbounds, nodebug, "guard_nodebug");
        builder.CreateCondBr(isInbounds, success, failure);
    }

    static void addPopToModule(Module &m, StgIRBuilder &builder, Function *F,
                               Value *stackTop, Value *stack,
                               Value *llvmNoDebug) {
        assert(F);
        assert(stackTop);
        assert(stack);
        F->addFnAttr(llvm::Attribute::InaccessibleMemOnly);

        BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
        builder.SetInsertPoint(entry);

        Value *idxPrev = builder.CreateLoad(stackTop, "idx");
        Value *idxCur =
            builder.CreateSub(idxPrev, builder.getInt64(1), "idx_dec");
        Value *isInbounds =
            builder.CreateICmpSGE(idxCur, builder.getInt64(0), "isGeqZero");
        Value *nodebug = builder.CreateLoad(llvmNoDebug, "nodebug");
        isInbounds = builder.CreateOr(isInbounds, nodebug, "guard_nodebug");

        // --success
        BasicBlock *success = BasicBlock::Create(m.getContext(), "success", F);
        builder.SetInsertPoint(success);
        Value *stackSlot =
            builder.CreateGEP(stack, {builder.getInt64(0), idxCur}, "slot");
        Value *Ret = builder.CreateLoad(stackSlot, "val");

        builder.CreateStore(idxCur, stackTop);
        builder.CreateRet(Ret);

        // -- failure
        BasicBlock *failure = BasicBlock::Create(m.getContext(), "failure", F);
        builder.SetInsertPoint(failure);
        sxhc::RuntimeDebugBuilder::createCPUPrinter(
            builder, "indexing stack negatively: ", F->getName(),
            "|cur: ", idxCur, "\n");
        Function *trap = getOrCreateFunction(
            m, FunctionType::get(builder.getVoidTy(), {}), "llvm.trap");
        trap->addFnAttr(llvm::Attribute::NoReturn);
        CallInst *CI = builder.CreateCall(trap, {});
        CI->setDoesNotReturn();
        builder.CreateUnreachable();
        // builder.CreateRet(UndefValue::get(stack->getType()->getPointerElementType()->getSequentialElementType()));

        // entry--
        builder.SetInsertPoint(entry);
        builder.CreateCondBr(isInbounds, success, failure);
    }

    static Function *addEnterDynamicClosureToModule(Module &m,
                                                    StgIRBuilder builder,
                                                    BuildCtx &bctx) {
        Function *F = createNewFunction(
            m,
            FunctionType::get(builder.getVoidTy(), {getRawMemTy(builder)},
                              /*varargs = */ false),
            "enter_dynamic_closure");

        // F->addFnAttr(Attribute::NoInline);
        F->addFnAttr(Attribute::AlwaysInline);
        F->setCallingConv(CallingConv::Fast);
        F->setLinkage(GlobalValue::InternalLinkage);

        BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
        builder.SetInsertPoint(entry);
        Argument *closureRaw = &*F->arg_begin();
        closureRaw->setName("closure_raw");

        Value *closureStruct = builder.CreateBitCast(
            closureRaw, bctx.ClosureTy[0]->getPointerTo());
        Value *contSlot = builder.CreateGEP(
            closureStruct, {builder.getInt64(0), builder.getInt32(0)},
            "cont_slot");
        LoadInst *cont = builder.CreateLoad(contSlot, "cont");
        cont->setMetadata(LLVMContext::MD_invariant_group,
                          bctx.getInvariantGroupNode());

        // Value *contAddr = builder.CreatePtrToInt(cont,
        // builder.getInt64Ty(), "cont_addr");

        // store the address of the closure we are entering
        // Value *closureAddr = builder.CreatePtrToInt(
        //     closureRaw, builder.getInt64Ty(), "closure_addr");
        // StoreInst *SI = builder.CreateStore(closureAddr,
        // bctx.enteringClosureAddr);
        // SI->setMetadata(LLVMContext::MD_invariant_group,
        // bctx.getInvariantGroupNode());

        // call the function
        CallInst *CI = builder.CreateCall(cont, {closureRaw});
        CI->setTailCallKind(CallInst::TCK_MustTail);
        CI->setCallingConv(CallingConv::Fast);
        builder.CreateRetVoid();
        return F;
    }

    // FTy :: StgIRBuilder -> Value* -> Value* -> Value*
    template <typename FTy>
    static LLVMClosureData *addPrimArithBinopToModule(std::string name,
                                                      FTy FResultBuilder,
                                                      Module &m,
                                                      StgIRBuilder builder,
                                                      BuildCtx &bctx) {
        Function *Dynamic = createNewFunction(
            m,
            FunctionType::get(builder.getVoidTy(), {builder.getInt8PtrTy()},
                              /*varargs = */ false),
            name);
        Dynamic->addFnAttr(llvm::Attribute::AlwaysInline);
        Dynamic->setCallingConv(CallingConv::Fast);
        Dynamic->setLinkage(GlobalValue::InternalLinkage);
        {
            BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", Dynamic);
            builder.SetInsertPoint(entry);
            Value *i = bctx.createPopInt(builder, "i");
            Value *j = bctx.createPopInt(builder, "j");
            Value *result = FResultBuilder(
                    builder, i, j);  // builder.CreateMul(i, j, "result");
            result->setName("result");

            bctx.createPushInt(builder, result);

            Value *RetFrame = bctx.createPopReturn(builder, "return_frame");
            materializeEnterDynamicClosure(RetFrame, m, builder, bctx);
            builder.CreateRetVoid();
        }


        Function *Static = createNewFunction(
            m,
            FunctionType::get(builder.getVoidTy(), {builder.getInt64Ty(), builder.getInt64Ty(), builder.getInt8PtrTy()},
                              /*varargs = */ false),
            name + "Static");

        Static->addFnAttr(llvm::Attribute::AlwaysInline);
        Static->setCallingConv(CallingConv::Fast);
        Static->setLinkage(GlobalValue::InternalLinkage);
        {
            BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", Static);
            builder.SetInsertPoint(entry);
            Value *i = Static->arg_begin();
            Value *j = Static->arg_begin() + 1;
            Value *result = FResultBuilder(
                    builder, i, j);  // builder.CreateMul(i, j, "result");
            result->setName("result");

            bctx.createPushInt(builder, result);

        }
        Value *RetFrame = bctx.createPopReturn(builder, "return_frame");
        materializeEnterDynamicClosure(RetFrame, m, builder, bctx);
        builder.CreateRetVoid();

        return new LLVMClosureData(materializeStaticClosure(
            Dynamic, Static, "closure_" + name, m, builder, bctx));
    }

    static Function *createBumpPointerAllocator(Module &m, StgIRBuilder builder,
                                                BuildCtx &bctx,
                                                Function *realMalloc) {
        GlobalVariable *rawHeapMemory = new GlobalVariable(
            m, builder.getInt8Ty()->getPointerTo(),
            /*isConstant=*/false, GlobalValue::ExternalLinkage,
            ConstantPointerNull::get(builder.getInt8Ty()->getPointerTo()),
            "rawHeapMemory");

        GlobalVariable *heapMemoryTop = new GlobalVariable(
            m, builder.getInt64Ty(),
            /*isConstant=*/false, GlobalValue::ExternalLinkage,
            ConstantInt::get(builder.getInt64Ty(), 0), "rawHeapMemoryTop");

        static const uint64_t HEAP_SIZE = 1ull /*bytes*/ * 1024ull /*kb*/ *
                                          1024ull /*mb*/ * 1024ull /*gn*/ *
                                          1ull * 10ull;
        std::cout << "HEAP_SIZE: " << HEAP_SIZE << "\n";
        static const uint64_t HEAP_SIZE_SAFETY = HEAP_SIZE - 1000;
        // static const uint64_t HEAP_SIZE_SAFETY = HEAP_SIZE - 512ull; // 1024
        // /*kb*/ * 1024 /*mb*/ * 1024 /*gb*/ * 5;
        std::cout << "HEAP_SIZE_SAFETY: " << HEAP_SIZE_SAFETY << "\n";

        // initialize for the global chunk of memory.
        {
            Function *memInitializer = createNewFunction(
                m, FunctionType::get(builder.getVoidTy(), false),
                "init_rawmem_constructor");
            appendToGlobalCtors(m, memInitializer, 0, rawHeapMemory);
            BasicBlock *entry =
                BasicBlock::Create(m.getContext(), "entry", memInitializer);
            builder.SetInsertPoint(entry);
            Value *valueHeapSize = builder.getInt64(HEAP_SIZE);
            Value *rawmem =
                builder.CreateCall(realMalloc, {valueHeapSize}, "rawmem");
            Value *mallocAddr = builder.CreatePtrToInt(
                rawmem, builder.getInt64Ty(), "malloc_addr");
            Value *isNull = builder.CreateICmpEQ(
                mallocAddr, builder.getInt64(0), "is_malloc_nullptr");

            BasicBlock *Success =
                BasicBlock::Create(m.getContext(), "success", memInitializer);
            BasicBlock *Failure = BasicBlock::Create(
                m.getContext(), "MallocNull", memInitializer);
            builder.CreateCondBr(isNull, Failure, Success);

            builder.SetInsertPoint(Success);
            builder.CreateStore(rawmem, rawHeapMemory);
            builder.CreateStore(builder.getInt64(0), heapMemoryTop);
            builder.CreateRetVoid();

            builder.SetInsertPoint(Failure);
            sxhc::RuntimeDebugBuilder::createCPUPrinter(
                builder, "malloc returned null for heap size: ", valueHeapSize,
                "\n");
            createCallTrap(m, builder);
            builder.CreateRetVoid();
        }

        {
            Function *alloc = createNewFunction(
                m,
                FunctionType::get(builder.getInt8Ty()->getPointerTo(),
                                  {builder.getInt64Ty()}, false),
                "bumpPointerAllocator");

            alloc->addFnAttr(Attribute::NoInline);
            alloc->addFnAttr(Attribute::InaccessibleMemOnly);
            alloc->setCallingConv(CallingConv::Fast);
            // alloc->setLinkage(GlobalValue::InternalLinkage);
            // this is fucked, but in a cool way: GC makes memory allocation
            // pure :) alloc->addFnAttr(Attribute::ReadNone); Why does readnone
            // fuck it up?

            BasicBlock *entry =
                BasicBlock::Create(m.getContext(), "entry", alloc);
            builder.SetInsertPoint(entry);

            Value *prevTop = builder.CreateLoad(heapMemoryTop, "prevTopSlot");
            Value *memBottom = builder.CreateLoad(rawHeapMemory, "memBottom");
            errs() << "memBottom: " << *memBottom << "\n";
            Value *mem = builder.CreateGEP(memBottom, prevTop, "memIndexed");
            errs() << "mem: " << *mem << "\n";

            Argument *memSize = alloc->arg_begin();
            memSize->setName("size");

            // size = floor((size + (ALIGNMENT - 1)) / ALIGNMENT) * ALIGNMENT
            // Value *sizeAligned = builder.CreateNUWMul(sizeBumped,
            // builder.getInt64(ALIGNMENT), "sizeAligned");
            static const int ALIGNMENT = 4;
            Value *sizeBumped = builder.CreateAdd(
                memSize, builder.getInt64(ALIGNMENT - 1), "sizeBumped");
            Value *sizeFloor = builder.CreateUDiv(
                sizeBumped, builder.getInt64(ALIGNMENT), "sizeFloored");
            Value *sizeAligned = builder.CreateNUWMul(
                sizeFloor, builder.getInt64(ALIGNMENT), "sizeAligned");
            Value *newTop = builder.CreateAdd(prevTop, sizeAligned, "newTop");
            builder.CreateStore(newTop, heapMemoryTop);

            Value *outOfMemory = builder.CreateICmpUGT(
                newTop, builder.getInt64(HEAP_SIZE_SAFETY), "outOfMemory");
            Value *nodebug = builder.CreateLoad(bctx.llvmNoDebug, "nodebug");
            Value *isdebug = builder.CreateNot(nodebug, "isdebug");
            outOfMemory =
                builder.CreateAnd(outOfMemory, isdebug, "outOfMemoryGuard");

            BasicBlock *retBB =
                BasicBlock::Create(m.getContext(), "ret", alloc);
            builder.SetInsertPoint(retBB);
            builder.CreateRet(mem);

            BasicBlock *trapBB =
                BasicBlock::Create(m.getContext(), "trap", alloc);
            builder.SetInsertPoint(trapBB);
            sxhc::RuntimeDebugBuilder::createCPUPrinter(
                builder, "blew past heap safety. Total heap size: " +
                             std::to_string(HEAP_SIZE) +
                             "bytes. Safety margin: " +
                             std::to_string(HEAP_SIZE_SAFETY) + "bytes.\n");
            Function *trap = getOrCreateFunction(
                m, FunctionType::get(builder.getVoidTy(), {}), "llvm.trap");
            builder.CreateCall(trap, {});
            builder.CreateRet(mem);

            builder.SetInsertPoint(entry);
            builder.CreateCondBr(outOfMemory, trapBB, retBB);

            return alloc;
        }
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
    assert(V->getType()->isPointerTy());
    if (V->getType() != getRawMemTy(builder)) {
        V = builder.CreateBitCast(V, getRawMemTy(builder));
    }

    CallInst *CI = builder.CreateCall(bctx.enterDynamicClosure, {V});
    errs() << "TODO: Removing musttail from " << __PRETTY_FUNCTION__ << " for experimentation.\n";
    // CI->setTailCallKind(CallInst::TCK_MustTail);
     CI->setTailCallKind(CallInst::TCK_Tail);
    CI->setCallingConv(llvm::CallingConv::Fast);
};

// As always, the one who organises things (calls the function) does the
// work: push params in reverse order.
void materializeApStaticallyUnknown(const ExpressionAp *ap,
                                    LLVMValueData fnValueData, Module &m,
                                    StgIRBuilder &builder, BuildCtx &bctx) {
    for (Atom *p : ap->params_reverse_range()) {
        Value *v = materializeAtom(p, builder, bctx);
        if (isa<AtomInt>(p)) {
            bctx.createPushInt(builder, v);
        } else {
            LLVMValueData vdata =
                bctx.getIdentifier(cast<AtomIdent>(p)->getIdent());
            if (vdata.stgtype == bctx.getPrimIntTy()) {
                bctx.createPushInt(builder, v);
            } else {
                bctx.createPushBoxed(builder, v);
            }
        }
    }

    if (fnValueData.stgtype == bctx.getPrimIntTy()) {
        materializeEnterInt(fnValueData.v, ap->getFnName(), m, builder, bctx);
    } else {
        materializeEnterDynamicClosure(fnValueData.v, m, builder, bctx);
    };
};

void materializeApStaticallyKnown(const ExpressionAp *ap,
                                  LLVMClosureData fnValueData, Module &m,
                                  StgIRBuilder &builder, BuildCtx &bctx) {
    std::cerr << "statically known ap: " << *ap << "\n";
    assert(fnValueData.getStaticCallFn());
    Function *Static = fnValueData.getStaticCallFn();
    errs() << "static fn: " << *fnValueData.getStaticCallFn() << "\n";
    // We need -1 for the closure struct passage.
    assert (ap->getNumParams() == -1 + fnValueData.getStaticCallFn()->getFunctionType()->getNumParams() && "currently only saturated function calls supported.");

    std::vector<Value *> Args;
    int i = 0;
    for (Atom *p : ap->params_range()) {
        Value *v = materializeAtom(p, builder, bctx);
        if (v->getType()->isPointerTy())
            v = builder.CreateBitCast(v, builder.getInt8PtrTy());
        errs() << __FUNCTION__ << " | V:" << *v << "\n";
        errs() << "v->getType(): " << *v->getType() << " | fn Arg:" << *Static->getFunctionType()->getParamType(i) << "\n";
        assert(v->getType() == Static->getFunctionType()->getParamType(i));
        Args.push_back(v);
        i++;
        // if (isa<AtomInt>(p)) {
        //     Args.push_back(v);
        // } else {
        //     LLVMValueData vdata =
        //         bctx.getIdentifier(cast<AtomIdent>(p)->getIdent());
        //     if (vdata.stgtype == bctx.getPrimIntTy()) {
        //         bctx.createPushInt(builder, v);
        //     } else {
        //         bctx.createPushBoxed(builder, v);
        //     }
        // }
    }
    errs() << "TODO: what to do with closure ptr?\n";
    for(Value *v : fnValueData.free_vars()) {
        assert(v->getType() == Static->getFunctionType()->getParamType(i));
        Args.push_back(v);
        i++;
    }
    // create dummy closure ptr.
    Args.push_back(llvm::UndefValue::get(builder.getInt8PtrTy()));
    CallInst *CI = builder.CreateCall(Static, Args);
    CI->setCallingConv(CallingConv::Fast);
};

void materializeAp(const ExpressionAp *ap, Module &m, StgIRBuilder &builder,
                   BuildCtx &bctx) {
    Optional<LLVMClosureData> staticallyKnownFnData =
        bctx.getTopLevelBindingFromName(ap->getFnName());
    if (staticallyKnownFnData && staticallyKnownFnData->getStaticCallFn() != nullptr) {
        materializeApStaticallyKnown(ap, *staticallyKnownFnData, m, builder,
                                     bctx);
    } else {
        LLVMValueData fnData = bctx.getIdentifier(ap->getFnName());
        materializeApStaticallyUnknown(ap, fnData, m, builder, bctx);
    }
};

void materializeConstructor(const ExpressionConstructor *c, Module &m,
                            StgIRBuilder &builder, BuildCtx &bctx) {
    // TODO: refactor this to use DataLayout.
    DataConstructor *cons;
    Type *structType;

    std::tie(cons, structType) = bctx.getDataConstructorFromName(c->getName());

    const int TotalSize =
        m.getDataLayout().getTypeAllocSize(structType);  // for the tag.

    // Value *rawMem = builder.CreateCall(bctx.malloc,
    //                                   {builder.getInt64(TotalSize)},
    //                                   "rawmem");
    // Value *typedMem =
    //    builder.CreateBitCast(rawMem, structType->getPointerTo(), "typedmem");
    Value *typedMem = bctx.createCallAllocate(builder, TotalSize, "constructor",
                                              structType->getPointerTo());

    const int Tag = cons->getParent()->getIndexForConstructor(cons);
    Value *tagIndex = builder.CreateGEP(
        typedMem, {builder.getInt64(0), builder.getInt32(0)}, "tag_index");
    StoreInst *SI = builder.CreateStore(builder.getInt64(Tag), tagIndex);
    SI->setMetadata(LLVMContext::MD_invariant_group,
                    bctx.getInvariantGroupNode());

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
        StoreInst *SI = builder.CreateStore(v, indexedMem);
        SI->setMetadata(LLVMContext::MD_invariant_group,
                        bctx.getInvariantGroupNode());
        i++;
    }

    bctx.createPushBoxed(builder, typedMem);
    // builder.CreateCall(bctx.pushBoxed, {typedMem});

    // now pop a continuation off the return stack and invoke it
    Value *ReturnCont = bctx.createPopReturn(builder, "returncont");

    // we need to use enterDynamicClosure because we create closures for our
    // return alts as well.
    materializeEnterDynamicClosure(ReturnCont, m, builder, bctx);
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
            LoadInst *LI =
                builder.CreateLoad(Slot, "cons_" + std::to_string(i));
            LI->setMetadata(LLVMContext::MD_invariant_group,
                            bctx.getInvariantGroupNode());
            bctx.insertIdentifier(var, LLVMValueData(LI, bctx.getPrimIntTy()));
        } else {
            LoadInst *LI = builder.CreateLoad(
                Slot, "cons_mem_as_int_" + std::to_string(i));
            LI->setMetadata(LLVMContext::MD_invariant_group,
                            bctx.getInvariantGroupNode());
            Value *V = builder.CreateIntToPtr(
                LI, getRawMemTy(builder), "cons_rawmem_" + std::to_string(i));
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
        } else if (isa<CaseAltInt>(a)) {
            assert(false && "unimplemented type deduction");
        } else if (isa<CaseAltVariable>(a)) {
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
        m,
        FunctionType::get(builder.getVoidTy(), {builder.getInt8PtrTy()},
                          /*isVarArg=*/false),
        "case_alt_" + scrutineeNameSS.str());
    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", f);
    builder.SetInsertPoint(entry);

    // **** COPY PASTE BEGIN
    // here is code duplication in some sense with
    // materializeDynamicLetBinding Open a new scope.
    BuildCtx::Scoper s(bctx);
    if (freeVarsInAlts.size() > 0) {
        // LoadInst *closureAddr =
        //     builder.CreateLoad(bctx.enteringClosureAddr, "closure_addr_int");
        //  closureAddr->setMetadata(LLVMContext::MD_invariant_group,
        //  bctx.getInvariantGroupNode());
        Value *closureRaw = &*f->arg_begin();
        closureRaw->setName("closure_raw");
        Value *closure = builder.CreateBitCast(
            closureRaw, bctx.ClosureTy[freeVarsInAlts.size()]->getPointerTo(),
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

    Value *rawmem = bctx.createPopBoxedVoidPtr(
        builder, "rawmem");  // builder.CreateCall(bctx.popBoxed, {}, "rawmem");

    Value *TagPtr = builder.CreateBitCast(
        rawmem, builder.getInt64Ty()->getPointerTo(), "tagptr");
    // Since we only care about the tag, we can convert to i64 and forget
    // about the rest.
    LoadInst *Tag = builder.CreateLoad(TagPtr, "tag");
    Tag->setMetadata(LLVMContext::MD_invariant_group,
                     bctx.getInvariantGroupNode());

    BasicBlock *failure = BasicBlock::Create(m.getContext(), "failure", f);
    builder.SetInsertPoint(failure);
    if (bctx.nodebug) {
        builder.CreateUnreachable();

    } else {
        Function *trap = getOrCreateFunction(
            m, FunctionType::get(builder.getVoidTy(), {}), "llvm.trap");
        builder.CreateCall(trap, {});
        builder.CreateRetVoid();
    }

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
                const StgType *ty =
                    getTypeOfExpression(c->getScrutinee(), bctx);
                BuildCtx::Scoper s(bctx);
                bctx.insertIdentifier(altVariable->getLHS(),
                                      LLVMValueData(rawmem, ty));
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
    errs() << "*" << __FUNCTION__ << "\n";
    std::stringstream namess;
    namess << "case_" << *c->getScrutinee() << "_alts";

    Function *F = createNewFunction(
        m,
        FunctionType::get(builder.getVoidTy(), {builder.getInt8PtrTy()},
                          /*isVarArg=*/false),
        namess.str());
    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(entry);

    // **** COPY PASTE BEGIN
    // here is code duplication in some sense with
    // materializeDynamicLetBinding Open a new scope.
    BuildCtx::Scoper s(bctx);
    if (freeVarsInAlts.size() > 0) {
        // LoadInst *closureAddr =
        //     builder.CreateLoad(bctx.enteringClosureAddr, "closure_addr_int");
        //  closureAddr->setMetadata(LLVMContext::MD_invariant_group,
        //  bctx.getInvariantGroupNode());
        Value *closureRaw = F->arg_begin();
        closureRaw->setName("closure_raw");

        Value *closure = builder.CreateBitCast(
            closureRaw, bctx.ClosureTy[freeVarsInAlts.size()]->getPointerTo(),
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
    Value *scrutinee = bctx.createPopInt(builder, "scrutinee");
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
        bctx.createPushInt(builder, scrutinee);
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

        if (bctx.nodebug) {
            builder.CreateUnreachable();
        } else {
            Function *trap = getOrCreateFunction(
                m, FunctionType::get(builder.getVoidTy(), {}), "llvm.trap");
            builder.CreateCall(trap, {});
            builder.CreateRetVoid();
        }
    }
    return F;
}

static const StgType *getTypeOfExpression(const Expression *e,
                                          const BuildCtx &bctx) {
    switch (e->getKind()) {
        case Expression::EK_Ap: {
            const ExpressionAp *ap = cast<ExpressionAp>(e);
            const std::string fnName = ap->getFnName();
            const StgType *CalledTy = bctx.getIdentifier(fnName).stgtype;
            if (CalledTy == bctx.getPrimIntTy()) return bctx.getPrimIntTy();

            const StgFunctionType *Fty = cast<StgFunctionType>(CalledTy);
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
        case Expression::EK_Let: {
            const ExpressionLet *let = cast<ExpressionLet>(e);

            // a let binding encloses all of the let defined
            // variables inside it, so we can remove those from the
            // free variables the let bindings uses.
            std::set<Identifier> bindingNames;
            std::set<Identifier> lambdaFree;
            for (const Binding *b : let->bindings_range()) {
                bindingNames.insert(b->getName());
            }

            for (const Binding *b : let->bindings_range()) {
                const Lambda *l = b->getRhs();
                for (const Parameter *p : l->free_params_range()) {
                    lambdaFree.insert(p->getName());
                }
            }
            std::set_difference(lambdaFree.begin(), lambdaFree.end(),
                                bindingNames.begin(), bindingNames.end(),
                                std::inserter(freeVars, freeVars.begin()));

            break;
        }
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
    continuation->addFnAttr(llvm::Attribute::AlwaysInline);
    continuation->setCallingConv(CallingConv::Fast);
    continuation->setLinkage(GlobalValue::InternalLinkage);

    Type *closureTy = bctx.ClosureTy[freeVarsInAlts.size()];
    Value *clsTyped = bctx.createCallAllocate(
        builder, m.getDataLayout().getTypeAllocSize(closureTy), "closure",
        closureTy->getPointerTo());

    // CallInst *clsRaw =
    //    builder.CreateCall(bctx.malloc,
    //                       {builder.getInt64(m.getDataLayout().getTypeAllocSize(
    //                           bctx.ClosureTy[freeVarsInAlts.size()]))},
    //                       "closure_raw");
    // clsRaw->addAttribute(AttributeList::ReturnIndex,
    // llvm::Attribute::NoAlias);  assert(clsRaw->returnDoesNotAlias());

    // Value *clsTyped = builder.CreateBitCast(
    //    clsRaw, bctx.ClosureTy[freeVarsInAlts.size()]->getPointerTo(),
    //    "closure_typed");
    Value *fnSlot = builder.CreateGEP(
        clsTyped, {builder.getInt64(0), builder.getInt32(0)}, "fn_slot");
    StoreInst *SI = builder.CreateStore(continuation, fnSlot);

    // MDBuilder mdbuilder(m.getContext());
    // MDNode *invariantGroup =
    // mdbuilder.createAnonymousAliasScopeDomain("closure_invariant_group");
    SI->setMetadata(LLVMContext::MD_invariant_group,
                    bctx.getInvariantGroupNode());

    // store free vars into the slot.
    int i = 0;
    for (Identifier freeVar : freeVarsInAlts) {
        Value *freeVarSlot = builder.CreateGEP(
            clsTyped,
            {builder.getInt64(0), builder.getInt32(1), builder.getInt32(i)},
            freeVar + "_free_in_case_slot");
        Value *freeVarVal = bctx.getIdentifier(freeVar).v;
        freeVarVal = TransmuteToInt(freeVarVal, builder);
        StoreInst *SI = builder.CreateStore(freeVarVal, freeVarSlot);
        //        SI->setAAMetadata(bctx.aliasctx.getCaseClosureScopeAAMD());
        SI->setMetadata(LLVMContext::MD_invariant_group,
                        bctx.getInvariantGroupNode());
        i++;
    }

    bctx.createPushReturn(builder, clsTyped);
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

// Copied from materializeEmptyTopLevelStaticBinding - consider merging with.
Value *_allocateLetBindingDynamicClosure(const Binding *b, BasicBlock *BB,
                                         Module &m, StgIRBuilder builder,
                                         BuildCtx &bctx) {
    const unsigned nFreeParams = b->getRhs()->free_params_size();
    assert(nFreeParams < bctx.MAX_FREE_PARAMS);

    Type *closureTy = bctx.ClosureTy[nFreeParams];
    builder.SetInsertPoint(BB);

    const uint64_t sizeInBytes = m.getDataLayout().getTypeAllocSize(closureTy);
    // no store to function slot, args. These come later.
    return bctx.createCallAllocate(builder, sizeInBytes, "let.dynamic.closure",
                                   closureTy->getPointerTo());
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

    LoadInst *LI = builder.CreateLoad(v, name);
    LI->setMetadata(LLVMContext::MD_invariant_group,
                    bctx.getInvariantGroupNode());
    v = LI;
    if (ty != bctx.getPrimIntTy()) {
        v = builder.CreateIntToPtr(v, getRawMemTy(builder), name + "_rawmem");
    }

    bctx.insertIdentifier(name, LLVMValueData(v, ty));
}

// Materialize the function that gets executed when a let-binding is
// evaluated. NOTE: this is exactly the same thing as
// materializeEmptyTopLevelStaticBinding, except that it also creates a static
// closure. Consider mergining with materializeEmptyTopLevelStaticBinding
Function *_materializeDynamicLetBinding(const Binding *b, Module &m,
                                        StgIRBuilder builder, BuildCtx &bctx) {
    FunctionType *FTy = FunctionType::get(
        builder.getVoidTy(), {builder.getInt8PtrTy()}, /*isVarArg=*/false);
    Function *F =
        Function::Create(FTy, GlobalValue::ExternalLinkage, b->getName(), &m);

    F->addFnAttr(llvm::Attribute::AlwaysInline);
    F->setLinkage(GlobalValue::InternalLinkage);
    F->setCallingConv(CallingConv::Fast);

    BasicBlock *entry = BasicBlock::Create(m.getContext(), "entry", F);
    builder.SetInsertPoint(entry);

    // LoadInst *closureAddr =
    //     builder.CreateLoad(bctx.enteringClosureAddr, "closure_addr_int");
    //  closureAddr->setMetadata(LLVMContext::MD_invariant_group,
    //  bctx.getInvariantGroupNode());
    Argument *closureRaw = &*F->arg_begin();
    closureRaw->setName("closure_raw");
    Value *closure = builder.CreateBitCast(
        closureRaw,
        bctx.ClosureTy[b->getRhs()->free_params_size()]->getPointerTo(),
        "closure_typed");

    // Value *closure = builder.CreateIntToPtr(
    //     closureAddr,
    //     bctx.ClosureTy[b->getRhs()->free_params_size()]->getPointerTo(),
    //     "closure_typed");
    BuildCtx::Scoper s(bctx);

    int i = 0;
    for (Parameter *p : b->getRhs()->free_params_range()) {
        const StgType *ty = bctx.getTypeFromRawType(p->getTypeRaw());
        loadFreeVariableFromClosure(closure, p->getName(), ty, i, builder,
                                    entry, bctx);
        i++;
    }
    materializeLambdaDynamic(b->getRhs(), m, builder, bctx);
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
    errs() << "*" << __FUNCTION__ << "\n";
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
        StoreInst *SI = builder.CreateStore(f, fnSlot);
        SI->setMetadata(LLVMContext::MD_invariant_group,
                        bctx.getInvariantGroupNode());

        int i = 0;
        for (Parameter *p : b->getRhs()->free_params_range()) {
            Value *freeParamSlot = builder.CreateGEP(
                cls,
                {builder.getInt64(0), builder.getInt32(1), builder.getInt32(i)},
                b->getName() + "_free_param_" + p->getName() + "_slot");
            Value *v = bctx.getIdentifier(p->getName()).v;
            v = TransmuteToInt(v, builder);
            StoreInst *SI = builder.CreateStore(v, freeParamSlot);
            SI->setMetadata(LLVMContext::MD_invariant_group,
                            bctx.getInvariantGroupNode());
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
    bctx.createPushInt(builder, v);
    Value *cont = bctx.createPopReturn(builder, "return_cont_" + contName);
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

void materializeLambdaStatic(const Lambda *l, Function *F, Module &m,
                             StgIRBuilder builder, BuildCtx &bctx) {
    assert(F);
    assert(!F->isDeclaration());
    builder.SetInsertPoint(&F->getEntryBlock());
    assert(l->free_params_size() == 0);
    int i = 0;
    for(Parameter *p : l->bound_params_range()) {
        const StgType *Ty = bctx.getTypeFromRawType(p->getTypeRaw());
        bctx.insertIdentifier(p->getName(), LLVMValueData(F->arg_begin() + i, Ty));
        // if (Ty == bctx.getPrimIntTy()) {
        //     bctx.insertIdentifier(p->getName(), LLVMValueData(F->arg_begin() + i, bctx.getPrimIntTy()));
        // }
        // else {
        //     bctx.insertIdentifier(p->getName(), LLVMValueData(F->arg_begin() + i, Ty));
        //     // report_fatal_error("unknown type for static lambda");
        // }
        i++;

    }
    materializeExpr(l->getRhs(), m, builder, bctx);
    // createCallTrap(m, builder);
    // builder.CreateRetVoid();
}

void materializeLambdaDynamic(const Lambda *l, Module &m, StgIRBuilder &builder,
                              BuildCtx &bctx) {
    Function *F = builder.GetInsertBlock()->getParent();
    assert(F);
    // F->dump();

    BuildCtx::Scoper scoper(bctx);
    for (const Parameter *p : l->bound_params_range()) {
        const StgType *Ty = bctx.getTypeFromRawType(p->getTypeRaw());
        if (Ty == bctx.getPrimIntTy()) {
            Value *pv = bctx.createPopInt(builder, "param_" + p->getName());
            bctx.insertIdentifier(p->getName(),
                                  LLVMValueData(pv, bctx.getPrimIntTy()));
        } else {
            // Value *pv =
            //    builder.CreateCall(bctx.popBoxed, {}, "param_" +
            //    p->getName());
            Value *pv =
                bctx.createPopBoxedVoidPtr(builder, "param_" + p->getName());
            bctx.insertIdentifier(p->getName(), LLVMValueData(pv, Ty));
        }
    }
    materializeExpr(l->getRhs(), m, builder, bctx);
}

// Create a static closure for a top-level function. That way, we don't need
// to repeatedly build the closure.
LLVMClosureData materializeStaticClosure(Function *DynamicCall, Function *StaticCall, std::string name,
                                              Module &m, StgIRBuilder &builder,
                                              BuildCtx &bctx) {
    StructType *closureTy = bctx.ClosureTy[0];

    // 2. Create the initializer for the closure
    Constant *initializer = [&] {
        return ConstantStruct::get(closureTy, {DynamicCall});
    }();

    GlobalVariable *closure =
        new GlobalVariable(m, closureTy, /*isconstant=*/true,
                           GlobalValue::ExternalLinkage, initializer, name);
    return LLVMClosureData(DynamicCall, StaticCall, closure, {});
}

// Create a top-level static binding from a "binding" that is parsed in STG.
LLVMClosureData materializeEmptyTopLevelStaticBinding(const Binding *b,
                                                      Module &m,
                                                      StgIRBuilder &builder,
                                                      BuildCtx &bctx) {
    assert(b->getRhs()->free_params_size() == 0 &&
           "top level bindings cannot have any free paramters.");
    FunctionType *DynamicTy = FunctionType::get(
        builder.getVoidTy(), {builder.getInt8PtrTy()}, /*isVarArg=*/false);
    Function *Dynamic =
        Function::Create(DynamicTy, GlobalValue::ExternalLinkage, b->getName(), &m);
    Dynamic->addFnAttr(llvm::Attribute::AlwaysInline);
    Dynamic->setCallingConv(CallingConv::Fast);
    if (b->getName() != "main")
        Dynamic->setLinkage(GlobalValue::InternalLinkage);


    std::vector<Type *> StaticArgTys;
    for(Parameter *p : b->getRhs()->bound_params_range()) {
        const StgType *StgTy = bctx.getTypeFromRawType(p->getTypeRaw());
        if (StgTy == bctx.getPrimIntTy()) {
            StaticArgTys.push_back(builder.getInt64Ty());
        }
        else {
            StaticArgTys.push_back(builder.getInt8PtrTy());
        }
    }
    // We need this for the closure ptr.
    StaticArgTys.push_back(builder.getInt8PtrTy());

    FunctionType *StaticTy = FunctionType::get(builder.getVoidTy(), StaticArgTys, /*isVarArgs=*/false);
    Function *Static = Function::Create(StaticTy, GlobalValue::ExternalLinkage, b->getName() + "Static", &m);
    Static->addFnAttr(llvm::Attribute::AlwaysInline);
    Static->setCallingConv(CallingConv::Fast);
    if (b->getName() != "main")
        Static->setLinkage(GlobalValue::InternalLinkage);
    return materializeStaticClosure(Dynamic, Static, b->getName() + "_closure", m,
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

void hackEliminateUnusedAlloc(Module &m, BuildCtx &bctx,
                              const int OPTION_OPTIMISATION_LEVEL) {
    Function *bumpPointer = bctx.getBumpPointerAllocator();
    Function *realMalloc = bctx.getRealMalloc();

    const std::string bumpPointerName = bumpPointer->getName();

    realMalloc->setName("__realmalloc");
    bumpPointer->setName("__" + bumpPointerName + "__");

    StgIRBuilder builder(m.getContext());

    assert(m.getFunction("malloc") == nullptr && "malloc still around!");
    Function *fakeMalloc = cast<Function>(m.getOrInsertFunction(
        "malloc", FunctionType::get(builder.getInt8PtrTy(0),
                                    {builder.getInt64Ty()}, false)));
    fakeMalloc->addFnAttr(llvm::Attribute::InaccessibleMemOnly);
    fakeMalloc->addFnAttr(llvm::Attribute::NoRecurse);
    fakeMalloc->addFnAttr(llvm::Attribute::NoUnwind);

    assert(bumpPointer->getType() == fakeMalloc->getType() &&
           "different types!");
    bumpPointer->replaceAllUsesWith(fakeMalloc);

    {
        PassBuilder PB;

        ModulePassManager MPM;
        FunctionPassManager FPM;
        CGSCCPassManager CGSCCPM;

        PassBuilder::OptimizationLevel optimisationLevel =
            static_cast<PassBuilder::OptimizationLevel>(
                (unsigned)PassBuilder::OptimizationLevel::O0 +
                OPTION_OPTIMISATION_LEVEL);

        if (optimisationLevel > 0) {
            MPM = PB.buildModuleSimplificationPipeline(optimisationLevel, PassBuilder::ThinLTOPhase::None);
            FPM = PB.buildFunctionSimplificationPipeline(
                optimisationLevel, PassBuilder::ThinLTOPhase::None);
        }

        LoopAnalysisManager LAM;
        FunctionAnalysisManager FAM;
        CGSCCAnalysisManager CGAM;
        ModuleAnalysisManager MAM;

        // Register the AA manager first so that our version is the one used.
        FAM.registerPass([&] { return PB.buildDefaultAAPipeline(); });

        PB.registerModuleAnalyses(MAM);
        PB.registerCGSCCAnalyses(CGAM);
        PB.registerFunctionAnalyses(FAM);
        PB.registerLoopAnalyses(LAM);
        PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

        // Fix the IR first, then run optimisations.
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        MPM.addPass(
            createModuleToPostOrderCGSCCPassAdaptor(std::move(CGSCCPM)));

        // We need to run the pipeline once for correctness. Anything after that
        // is optimisation.
        MPM.run(m, MAM);
    }

    if ((fakeMalloc = m.getFunction("malloc"))) {
        assert(fakeMalloc != nullptr && "unable to find malloc.");
        assert(bumpPointer->getType() == fakeMalloc->getType() &&
               "different types!");
        fakeMalloc->replaceAllUsesWith(bumpPointer);
        fakeMalloc->eraseFromParent();
    } else {
    }

    bumpPointer->setName(bumpPointerName);
    realMalloc->setName("malloc");
}

// HACK: C++ does not allow compile-time string literals without hoop-jumping.
struct StackNames {
    static const char ReturnStackName[];
    static const char IntStackName[];
};

const char StackNames::ReturnStackName[] = "Return";
const char StackNames::IntStackName[] = "Int";

// HACK: Why do I need this? Why is the declaration in StackAnalysis.cpp not
// sufficient? template<> llvm::AnalysisKey
// StackAnalysisPass<StackNames::ReturnStackName>::Key; template<>
// llvm::AnalysisKey StackAnalysisPass<StackNames::IntStackName>::Key;

// template<>
// llvm::AnalysisKey *StackAnalysisPass<StackNames::ReturnStackName>::ID() {
//    return &StackAnalysisPass<StackNames::ReturnStackName>::Key;
//}
//
// template<>
// llvm::AnalysisKey *StackAnalysisPass<StackNames::IntStackName>::ID() {
//    return &StackAnalysisPass<StackNames::IntStackName>::Key;
//}

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
    const int OPTION_OPTIMISATION_LEVEL = [&opts] {
        if (!opts.count("O")) return 0;
        int opt = opts["O"].as<int>();
        assert(opt >= 0 && "-O levels can only be -O{0, 1, 2, 3}");
        assert(opt <= 3 && "-O levels can only be -O{0, 1, 2, 3}");
        return opt;
    }();

    const TargetMachine::CodeGenFileType OPTION_CODEGEN_FILE_TYPE =
        opts.count("emit-asm") ? TargetMachine::CGFT_AssemblyFile
                               : TargetMachine::CGFT_ObjectFile;

    static LLVMContext ctx;
    static StgIRBuilder builder(ctx);

    std::unique_ptr<Module> m(new Module("Module", ctx));

    cerr << "----\n";
    cerr << "Source program: ";
    cerr << *program << "\n";
    cerr << "----\n";
    // return 0;

    BuildCtx *bctxPtr =
        new BuildCtx(*m, builder, /*nodebug=*/OPTION_OPTIMISATION_LEVEL > 0);
    BuildCtx &bctx = *bctxPtr;
    Binding *entrystg = nullptr;
    for (DataType *datatype : program->datatypes_range()) {
        assert(datatype->constructors_size() > 0);
        bctx.insertType(datatype->getTypeName(), new StgDataType(datatype));
        for (DataConstructor *cons : datatype->constructors_range()) {
            bctx.insertDataConstructor(
                cons->getName(), cons,
                materializeDataConstructor(datatype, cons, *m, builder, bctx));
        }
    }

    // First, create empty top level bindings and insert them.
    // This is so that when we codegen the bindings, we can have recusion,
    // mututal recursion, and all that fun stuff.
    std::map<Binding *, LLVMClosureData> bindingToClosure;
    for (Binding *b : program->bindings_range()) {
        if (b->getName() == "main") {
            assert(!entrystg && "program has more than one main.");
            entrystg = b;
        }
        const LLVMClosureData cls =
            materializeEmptyTopLevelStaticBinding(b, *m, builder, bctx);
        bindingToClosure.insert(std::make_pair(b, cls));

        bctx.insertTopLevelBinding(
            b->getName(), createStgTypeForLambda(b->getRhs(), bctx), cls);
    }

    for (auto It : bindingToClosure) {
        Function *F = It.second.getDynamicCallFn();
        const Binding *b = It.first;
        BasicBlock *entry = BasicBlock::Create(m->getContext(), "entry", F);
        builder.SetInsertPoint(entry);
        materializeLambdaDynamic(b->getRhs(), *m, builder, bctx);
        builder.CreateRetVoid();
    }

    for (auto It : bindingToClosure) {
        Function *F = It.second.getStaticCallFn();
        const Binding *b = It.first;
        BasicBlock *entry = BasicBlock::Create(m->getContext(), "entry", F);
        builder.SetInsertPoint(entry);
        materializeLambdaStatic(b->getRhs(), F, *m, builder, bctx);
        builder.CreateRetVoid();
    }

    {
        PassBuilder PB;

        ModulePassManager MPM;
        FunctionPassManager FPM;
        CGSCCPassManager CGSCCPM;

        PassBuilder::OptimizationLevel optimisationLevel =
            static_cast<PassBuilder::OptimizationLevel>(
                (unsigned)PassBuilder::OptimizationLevel::O0 +
                OPTION_OPTIMISATION_LEVEL);

        if (optimisationLevel > PassBuilder::OptimizationLevel::O0) {
            MPM = PB.buildModuleOptimizationPipeline(optimisationLevel);
            FPM = PB.buildFunctionSimplificationPipeline(
                optimisationLevel, PassBuilder::ThinLTOPhase::None);
            FPM.addPass(StackMatcherPass<StackNames::ReturnStackName>());
            FPM.addPass(StackMatcherPass<StackNames::IntStackName>());
            // FPM.addPass(SinkPushPass("Return"));
        }

        LoopAnalysisManager LAM;
        FunctionAnalysisManager FAM;
        CGSCCAnalysisManager CGAM;
        ModuleAnalysisManager MAM;

        // Register the AA manager first so that our version is the one used.
        FAM.registerPass([&] { return PB.buildDefaultAAPipeline(); });
        FAM.registerPass(
            [&] { return StackAnalysisPass<StackNames::IntStackName>(); });
        FAM.registerPass(
            [&] { return StackAnalysisPass<StackNames::ReturnStackName>(); });

        PB.registerModuleAnalyses(MAM);
        PB.registerCGSCCAnalyses(CGAM);
        PB.registerFunctionAnalyses(FAM);
        PB.registerLoopAnalyses(LAM);
        PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

        // Fix the IR first, then run optimisations.
        MPM.addPass(AlwaysInlinerPass());
        MPM.addPass(createModuleToFunctionPassAdaptor(std::move(FPM)));
        MPM.addPass(
            createModuleToPostOrderCGSCCPassAdaptor(std::move(CGSCCPM)));

        for (Function &F : *m) {
            if (&F == bctx.getBumpPointerAllocator()) continue;
            if (F.getName().count("push")) continue;
            if (F.getName().count("pop")) continue;
            F.removeFnAttr(llvm::Attribute::NoInline);
            F.addFnAttr(llvm::Attribute::AlwaysInline);
        }

        if (optimisationLevel > 0) {
            for (int i = 0; i < 50; i++) {
                MPM.run(*m, MAM);
                hackEliminateUnusedAlloc(*m, bctx, optimisationLevel);
            }
        }
    }

    delete bctxPtr;

    if (verifyModule(*m, nullptr) == 1) {
        cerr << "-----\n";
        cerr << "Module(Post optimisations):\n";
        errs() << *m << "\n";
        cerr << "-----\n";
        cerr << " *** Broken module found, aborting compilation.\nError:\n";
        verifyModule(*m, &errs());
        exit(1);
    }


    // Setup for codegen
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
    llvm::TargetMachine *TM = Target->createTargetMachine(
            TargetTriple, CPU, Features, opt, RM);
    m->setDataLayout(TM->createDataLayout());




    std::error_code errcode;
    llvm::raw_fd_ostream outputFile(OPTION_OUTPUT_FILENAME, errcode,
                                    llvm::sys::fs::F_None);

    if (errcode) {
        std::cerr << "Unable to open output file: " << OPTION_OUTPUT_FILENAME
                  << "\n";
        std::cerr << "Error: " << errcode.message() << "\n";
        exit(1);
    }

    if (OPTION_DUMP_LLVM) {
        m->print(outputFile, nullptr);
    } else {
        if (OPTION_OUTPUT_FILENAME == "-" &&
            OPTION_CODEGEN_FILE_TYPE == TargetMachine::CGFT_ObjectFile) {
            errs() << "WARNING: trying to print an object file to stdout, this "
                      "will be ugly. skipping because this is pointless for "
                      "now.\n";
            errs() << "To print LLVM IR, use --emit-llvm. To print assembly, "
                      "use --emit-asm\n";
        } else {
            legacy::PassManager PM;
            if (TM->addPassesToEmitFile(PM, outputFile, nullptr,
                                        OPTION_CODEGEN_FILE_TYPE)) {
                report_fatal_error(
                    "Target machine can't emit a file of this type.");
            }
            PM.run(*m);
        }
    }
    outputFile.flush();

    if (OPTION_JIT) {
        errs() << "---------\n";
        errs() << "JIT: executing module:\n";
        llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);

        std::unique_ptr<SimpleJIT> jit = std::move(SimpleJIT::Create().get());
        llvm::sys::DynamicLibrary::LoadLibraryPermanently(nullptr);

        // NOTE: I don't *atually* want to move the damn module, wtf
        // assert(false && "Moving the module, how do I clone the module?");
        jit->addModule(llvm::CloneModule(*m));

        Expected<JITEvaluatedSymbol> memConstructor =
            jit->lookup("init_rawmem_constructor");
        // if (!memConstructor) {
        //     m->print(errs(), nullptr);
        //     errs() << "unable to find `init_rawmem_constructor` in given "
        //               "module:^\n";
        //     exit(1);
        // };

        SimplexhcInitRawmemConstructorTy rawmemConstructor =
            (SimplexhcInitRawmemConstructorTy)(memConstructor.get().getAddress());
        rawmemConstructor();

        Expected<JITTargetAddress> maybeMain =
            jit->lookup("main").get().getAddress();
        if (!maybeMain) {
            errs() << "unable to find `main` in given module:\n";
            m->print(outs(), nullptr);
            exit(1);
        }

        SimplexhcMainTy main = (SimplexhcMainTy)maybeMain.get();
        std::cout.flush();
        main();
        std::cout.flush();
        outs() << "\n----------------\n";
    }

    return 0;
}
