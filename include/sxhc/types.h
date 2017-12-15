#pragma once
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Value.h"
#include "stgir.h"
#include <sstream>
#include <string>
#include <vector>
#include <iostream>
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/ADT/ArrayRef.h"

using StgIRBuilder = llvm::IRBuilder<>;

class StgType {
   public:
    enum StgTypeKind { STK_Data, STK_Function };
    StgTypeKind getKind() const { return kind; }
    virtual std::string getTypeName() const = 0;
    virtual void dump() const { std::cout << getTypeName(); }

   private:
    StgTypeKind kind;

   protected:
    StgType(StgTypeKind kind) : kind(kind){};
};

class StgDataType : public StgType {
   private:
    const stg::DataType *datatype;

   public:
    explicit StgDataType(const stg::DataType *datatype)
        : StgType(StgType::STK_Data), datatype(datatype){};
    const stg::DataType *getDataType() const { return datatype; };

    std::string getTypeName() const { return datatype->getTypeName(); }

    static bool classof(const StgType *ty) { return ty->getKind() == STK_Data; }
};

class StgFunctionType : public StgType {
   private:
    const StgType *returnType;
    llvm::SmallVector<const StgType *, 4> paramTypes;

   public:
    StgFunctionType(const StgType *returnType,
                    llvm::ArrayRef<const StgType *> paramTypesref)
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


class LLVMClosureData {
   public:
       using FreeVarsTy = std::vector<llvm::Value *>;
       using iterator = FreeVarsTy::iterator;

    LLVMClosureData(llvm::Function *dynamicCallFn, llvm::Function *staticCallFn,
                    llvm::Value *closure, const std::vector<llvm::Value *> &freeVars)
        : dynamicCallFn(dynamicCallFn),
          staticCallFn(staticCallFn),
          closure(closure), freeVars(freeVars.begin(), freeVars.end()) {
          };
    llvm::Function *getDynamicCallFn() { return dynamicCallFn; }
    llvm::Function *getStaticCallFn() { return staticCallFn; }
    llvm::Value *getClosure() { return closure; }

    unsigned size() const { return freeVars.size(); }
    iterator free_begin() { return freeVars.begin(); }
    iterator free_end() { return freeVars.end(); }
    llvm::iterator_range<iterator> free_vars() { return make_range(free_begin(), free_end()); }

   private:
    llvm::Function *dynamicCallFn;
    llvm::Function *staticCallFn;
    llvm::Value *closure;
    FreeVarsTy freeVars;
};

struct LLVMValueData {
    llvm::Value* v;
    const StgType *stgtype;

    LLVMValueData(llvm::Value *v, const StgType *stgtype) : v(v), stgtype(stgtype) {}
};

