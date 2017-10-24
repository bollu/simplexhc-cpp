#pragma once
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <iostream>

#include <llvm/Support/Casting.h>

namespace stg {

using namespace llvm;
using Identifier = std::string;
using ConstructorName = std::string;
using TypeName = std::string;

// *** Data declaration
class DataType;
class DataConstructor {
   public:
    using TypeList = SmallVector<TypeName *, 4>;

   private:
    friend class DataType;
    ConstructorName name;
    TypeList types;

    const DataType *parent;
    void _setParent(DataType *parent) {
        this->parent = parent;
        assert(parent);
    }

   public:
    DataConstructor(ConstructorName name, ArrayRef<TypeName *> typesref)
        : name(name) {
        for (TypeName *t : typesref) {
            types.push_back(t);
        }
    }
    ConstructorName getName() const { return name; }

    using iterator = TypeList::iterator;
    using const_iterator = TypeList::const_iterator;

    iterator types_begin() { return types.begin(); }
    iterator types_end() { return types.end(); }

    const_iterator types_begin() const { return types.begin(); }
    const_iterator types_end() const { return types.end(); }

    const TypeName getTypeName(size_t i) const { return *types[i]; }

    iterator_range<iterator> types_range() {
        return make_range<iterator>(types_begin(), types_end());
    };
    iterator_range<const_iterator> types_range() const {
        return make_range<const_iterator>(types_begin(), types_end());
    };

    size_t types_size() const { return types.size(); }
    bool types_empty() const { return types.empty(); }

    const DataType *getParent() const {
        assert(parent);
        return this->parent;
    }

    void print(std::ostream &os) const;
    friend std::ostream &operator<<(std::ostream &os,
                                    const DataConstructor &branch);
};

class DataType {
   public:
    using DataConstructorList = SmallVector<DataConstructor *, 4>;
    using iterator = DataConstructorList::iterator;
    using const_iterator = DataConstructorList::const_iterator;

   private:
    DataConstructorList constructors;
    TypeName name;

   public:
    DataType(TypeName name, ArrayRef<DataConstructor *> consref) : name(name) {
        for (DataConstructor *c : consref) {
            c->_setParent(this);
            constructors.push_back(c);
        }
    }
    iterator begin() { return constructors.begin(); }
    const_iterator begin() const { return constructors.begin(); }

    iterator end() { return constructors.end(); }
    const_iterator end() const { return constructors.end(); }
    size_t constructors_size() const { return constructors.size(); }

    TypeName getTypeName() const { return name; }

    iterator_range<iterator> constructors_range() {
        return make_range(begin(), end());
    }
    iterator_range<const_iterator> constructors_range() const {
        return make_range(begin(), end());
    }

    void print(std::ostream &os) const;
    friend std::ostream &operator<<(std::ostream &os, const DataType &decl);
    unsigned getIndexForConstructor(const DataConstructor *needle) const {
        for (int i = 0; i < constructors.size(); i++) {
            if (constructors[i] == needle) return i;
        }

        report_fatal_error(
            "unknown data constructor variant asked for data declaration");
    }
};

// *** Atom ***
class Atom {
   public:
    enum AtomKind { AK_Int, AK_Ident };

    AtomKind getKind() const { return kind; }
    virtual void print(std::ostream &os) const = 0;

    friend std::ostream &operator<<(std::ostream &os, const Atom &a);

   protected:
    Atom(AtomKind kind) : kind(kind){};

   private:
    const AtomKind kind;
};

class AtomInt : public Atom {
    int val;

   public:
    AtomInt(int val) : Atom(Atom::AK_Int), val(val) {}
    void print(std::ostream &os) const;
    static bool classof(const Atom *S) { return S->getKind() == Atom::AK_Int; }
    int getVal() const { return val; }
};

class AtomIdent : public Atom {
    Identifier ident;

   public:
    AtomIdent(std::string ident) : Atom(Atom::AK_Ident), ident(ident) {}
    void print(std::ostream &os) const;

    Identifier getIdent() const { return ident; };

    static bool classof(const Atom *S) {
        return S->getKind() == Atom::AK_Ident;
    }
};

// *** Expression ****
class Expression {
   public:
    enum ExpressionKind { EK_Ap, EK_Cons, EK_Case, EK_Let, EK_IntLiteral };
    virtual void print(std::ostream &os) const = 0;
    ExpressionKind getKind() const { return kind; }

    friend std::ostream &operator<<(std::ostream &os, const Expression &e) {
        e.print(os);
        return os;
    }

   protected:
    Expression(ExpressionKind kind) : kind(kind){};

   private:
    const ExpressionKind kind;
};

class ExpressionAp : public Expression {
   public:
    using ParamsTy = SmallVector<Atom *, 2>;
    using const_iterator = ParamsTy::const_iterator;
    using const_reverse_iterator = ParamsTy::const_reverse_iterator;

    const_iterator params_begin() const { return args.begin(); }
    const_iterator params_end() const { return args.end(); }

    iterator_range<const_iterator> params_range() const {
        return make_range(params_begin(), params_end());
    }

    iterator_range<const_reverse_iterator> params_reverse_range() const {
        return make_range(params_end(), params_begin());
    }
    ExpressionAp(Identifier fn, std::initializer_list<Atom *> args)
        : Expression(Expression::EK_Ap), fn(fn), args(args){};

    ExpressionAp(Identifier fn, ArrayRef<Atom *> argsref)
        : Expression(Expression::EK_Ap), fn(fn) {
        for (Atom *arg : argsref) args.push_back(arg);
    };
    std::string getFnName() const { return fn; }

    void print(std::ostream &os) const;

    static bool classof(const Expression *E) {
        return E->getKind() == Expression::EK_Ap;
    }

   private:
    Identifier fn;
    ParamsTy args;
};

class ExpressionConstructor : public Expression {
   public:
    using ArgsTy = SmallVector<Atom *, 2>;

   private:
    ConstructorName name;
    SmallVector<Atom *, 2> args;

   public:
    using iterator = ArgsTy::iterator;
    using const_iterator = ArgsTy::const_iterator;

    iterator args_begin() { return args.begin(); }
    const_iterator args_begin() const { return args.begin(); }

    iterator args_end() { return args.end(); }
    const_iterator args_end() const { return args.end(); }

    iterator_range<iterator> args_range() {
        return make_range(args_begin(), args_end());
    }
    iterator_range<const_iterator> args_range() const {
        return make_range(args_begin(), args_end());
    }

    ExpressionConstructor(ConstructorName name,
                          std::initializer_list<Atom *> args)
        : Expression(Expression::EK_Cons), name(name), args(args){};

    ExpressionConstructor(ConstructorName name, ArrayRef<Atom *> argsref)
        : Expression(Expression::EK_Cons), name(name) {
        for (Atom *arg : argsref) args.push_back(arg);
    };

    ConstructorName getName() const { return name; };
    void print(std::ostream &os) const;
    static bool classof(const Expression *E) {
        return E->getKind() == Expression::EK_Cons;
    }
};

class Binding;
class ExpressionLet : public Expression {
   public:
    using BindingListTy = SmallVector<Binding *, 2>;
    using iterator = BindingListTy::iterator;
    using const_iterator = BindingListTy::const_iterator;

   private:
    BindingListTy bindings;
    Expression *rhs;

   public:
    ExpressionLet(ArrayRef<Binding *> bindingsref, Expression *rhs)
        : Expression(Expression::EK_Let), rhs(rhs) {
        for (Binding *b : bindingsref) {
            bindings.push_back(b);
        }
    }
    void print(std::ostream &os) const;
    const Expression *getRHS() const { return rhs; }
    const_iterator begin() const { return bindings.begin(); }
    const_iterator end() const { return bindings.end(); }
    iterator_range<const_iterator> bindings_range() const {
        return make_range(begin(), end());
    }
    static bool classof(const Expression *E) {
        return E->getKind() == Expression::EK_Let;
    }
};
    class ExpressionIntLiteral : public Expression {
    private:
        int value;
    public:
      ExpressionIntLiteral(int value)
          : Expression(Expression::EK_IntLiteral), value(value) {}

      int getValue() const { return value; }

        static bool classof(const Expression *E) {
          return E->getKind() == Expression::EK_IntLiteral;
        }
        void print(std::ostream &os) const;
    };


// *** Alt ***
class CaseAlt {
   public:
    Expression *getRHS() { return rhs; }
    const Expression *getRHS() const { return rhs; }

    friend std::ostream &operator<<(std::ostream &os, const CaseAlt &a);
    virtual void print(std::ostream &os) const = 0;

    enum CaseAltKind { CAK_Int, CAK_Variable, CAK_Destructure, CAK_Default };
    CaseAltKind getKind() const { return kind; }

   private:
    CaseAltKind kind;

   protected:
    Expression *rhs;
    CaseAlt(CaseAltKind kind, Expression *rhs) : kind(kind), rhs(rhs){};
};

class CaseAltInt : public CaseAlt {
    AtomInt *lhs;

   public:
    CaseAltInt(AtomInt *lhs, Expression *rhs)
        : CaseAlt(CAK_Int, rhs), lhs(lhs){};
    static bool classof(const CaseAlt *a) {
        return a->getKind() == CaseAlt::CAK_Int;
    }

    int getLHS() const {
        return lhs->getVal();
    }

    void print(std::ostream &os) const;
};

class CaseAltVariable : public CaseAlt {
    Identifier lhs;

   public:
    CaseAltVariable(Identifier lhs, Expression *rhs)
        : CaseAlt(CAK_Variable, rhs), lhs(lhs){};
    static bool classof(const CaseAlt *a) {
        return a->getKind() == CaseAlt::CAK_Variable;
    }
    Identifier getLHS() const { return lhs; }
    void print(std::ostream &os) const;
};

// case * of { __Constrcutor x y z -> f(x, y, z) }
class CaseAltDestructure : public CaseAlt {
   public:
    using VariableList = SmallVector<Identifier, 4>;
    using iterator = VariableList::iterator;
    using const_iterator = VariableList::const_iterator;

   private:
    ConstructorName constructorName;
    VariableList vars;

   public:
    CaseAltDestructure(ConstructorName constructorName,
                       ArrayRef<Identifier> varsref, Expression *rhs)
        : CaseAlt(CAK_Destructure, rhs), constructorName(constructorName) {
        for (Identifier var : varsref) vars.push_back(var);
    }

    const_iterator begin() const { return vars.begin(); }
    const_iterator end() const { return vars.end(); }
    iterator_range<const_iterator> variables_range() const {
        return make_range(begin(), end());
    }

    size_t variables_size() const { return vars.size(); }
    static bool classof(const CaseAlt *a) {
        return a->getKind() == CaseAlt::CAK_Destructure;
    }
    void print(std::ostream &os) const;

    ConstructorName getConstructorName() const { return constructorName; }
};

class CaseAltDefault : public CaseAlt {
   public:
    CaseAltDefault(Expression *rhs) : CaseAlt(CAK_Default, rhs){};
    void print(std::ostream &os) const;
    static bool classof(const CaseAlt *a) {
        return a->getKind() == CaseAlt::CAK_Default;
    }
};

// *** Case *** //
class ExpressionCase : public Expression {
   public:
    using AltsList = SmallVector<CaseAlt *, 2>;

   private:
    Expression *scrutinee;
    AltsList alts;

   public:
    ExpressionCase(Expression *scrutinee, ArrayRef<CaseAlt *> altsref)
        : Expression(Expression::EK_Case), scrutinee(scrutinee) {
        for (CaseAlt *alt : altsref) {
            alts.push_back(alt);
        }

        bool hasDefaultAlt = false;
        for (const CaseAlt *alt : this->alts) {
            if (isa<CaseAltDefault>(alt)) {
                assert(!hasDefaultAlt && "Case expression has multiple default alts!");
                hasDefaultAlt = true;
            }
        }

        bool hasVariableAlt = false;
        for (const CaseAlt *alt : this->alts) {
            if (isa<CaseAltVariable>(alt)) {
                assert(!hasVariableAlt && "Case expression has multiple variable alts!");
                hasVariableAlt = true;
            }
        }

        if (hasDefaultAlt && hasVariableAlt) {
            assert(false && "case has *both* default *and* variable alt, cannot compile!");
        }

    }

    void print(std::ostream &os) const;
    static bool classof(const Expression *E) {
        return E->getKind() == Expression::EK_Case;
    }

    const Expression *getScrutinee() const { return scrutinee; }

    using iterator = AltsList::iterator;
    using const_iterator = AltsList::const_iterator;

    const_iterator alts_begin() const { return alts.begin(); }
    const_iterator alts_end() const { return alts.end(); }
    size_t alts_size() const { return alts.size(); }

    iterator_range<const_iterator> alts_range() const {
        return make_range(alts_begin(), alts_end());
    }

    const CaseAltDefault *getDefaultAlt()  const {
        for (const CaseAlt* alt : this->alts) {
            if (const CaseAltDefault *d = dyn_cast<CaseAltDefault>(alt)) {
                return d;
            }
        }
        return nullptr;
    }

    const CaseAltVariable *getVariableAlt()  const {
        for (const CaseAlt* alt : this->alts) {
            if (const CaseAltVariable *v = dyn_cast<CaseAltVariable>(alt)) {
                return v;
            }
        }
        return nullptr;
    }
};

// *** Parameter ***
class Parameter {
    Identifier name;
    TypeName type;

   public:
    Parameter(Identifier name, TypeName type) : name(name), type(type){};
    void print(std::ostream &os) const;
    friend std::ostream &operator<<(std::ostream &os, const Parameter &p);

    TypeName getTypeName() const { return type; }
    Identifier getName() const { return name; }
};

// *** Lambda ***
class Lambda {
   public:
    using ParamList = SmallVector<Parameter *, 4>;
    using iterator = ParamList::iterator;
    using const_iterator = ParamList::const_iterator;
    using reverse_iterator = ParamList::reverse_iterator;
    using const_reverse_iterator = ParamList::const_reverse_iterator;

   private:
    ParamList boundparams;
    ParamList freeparams;
    Expression *expr;
    TypeName returnType;

   public:
    Lambda(ArrayRef<Parameter *> boundparamsref, TypeName returnType,
           Expression *expr)
        : expr(expr), returnType(returnType) {
        for (Parameter *p : boundparamsref) {
            boundparams.push_back(p);
        }
    }

    Lambda(ArrayRef<Parameter *> freeparamsref,
           ArrayRef<Parameter *> boundparamsref, TypeName returnType,
           Expression *expr)
        : expr(expr), returnType(returnType) {
        for (Parameter *p : freeparamsref) {
            freeparams.push_back(p);
        }
        for (Parameter *p : boundparamsref) {
            boundparams.push_back(p);
        }
    }
    void print(std::ostream &os) const;
    friend std::ostream &operator<<(std::ostream &os, const Lambda &l);

    const Expression *getRhs() const { return expr; }

    const_iterator bound_params_begin() const { return boundparams.begin(); }
    const_iterator bound_params_end() const { return boundparams.end(); }

    iterator_range<const_iterator> bound_params_range() const {
        return make_range(bound_params_begin(), bound_params_end());
    }

    const_iterator free_params_begin() const { return freeparams.begin(); }
    const_iterator free_params_end() const { return freeparams.end(); }
    unsigned free_params_size() const { return freeparams.size(); }

    iterator_range<const_iterator> free_params_range() const {
        return make_range(free_params_begin(), free_params_end());
    }

    ArrayRef<const Parameter *>free_params_ref() const { return freeparams; }

    iterator_range<const_reverse_iterator> free_params_reverse_range() const {
        return make_range(free_params_end(), free_params_begin());
    }

    std::string getReturnTypeName() const {
        return this->returnType;
    }
};

// *** Binding ***
class Binding {
    Identifier lhs;
    Lambda *rhs;

   public:
    Binding(Identifier lhs, Lambda *rhs) : lhs(lhs), rhs(rhs){};
    friend std::ostream &operator<<(std::ostream &os, const Binding &b);
    Identifier getName() const { return lhs; }
    const Lambda *getRhs() const { return rhs; }
};

// *** Program ***
class Program {
   public:
    Program(ArrayRef<Binding *> bs, ArrayRef<DataType *> ds) {
        for (Binding *b : bs) bindings.push_back(b);
        for (DataType *d : ds) datatypes.push_back(d);
    };

    using DataTypeList = SmallVector<DataType *, 4>;
    using BindingList = SmallVector<Binding *, 4>;

    using binding_iterator = BindingList::iterator;
    using datatype_iterator = DataTypeList::iterator;

    friend std::ostream &operator<<(std::ostream &os, const Program &p);

    binding_iterator bindings_begin() { return bindings.begin(); }
    binding_iterator bindings_end() { return bindings.end(); }

    iterator_range<binding_iterator> bindings_range() {
        return make_range(bindings_begin(), bindings_end());
    }

    datatype_iterator datatypes_begin() { return datatypes.begin(); }
    datatype_iterator datatypes_end() { return datatypes.end(); }

    iterator_range<datatype_iterator> datatypes_range() {
        return make_range(datatypes_begin(), datatypes_end());
    }

   private:
    BindingList bindings;
    DataTypeList datatypes;
};

}  // end namespace stg.
