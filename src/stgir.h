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
class DataDeclaration {
   public:
    using TypeList = SmallVector<TypeName *, 4>;

   private:
    ConstructorName name;
    TypeList types;

   public:
    DataDeclaration(ConstructorName name, ArrayRef<TypeName *> typesref)
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

    iterator_range<iterator> types_range() {
        return make_range<iterator>(types_begin(), types_end());
    };
    iterator_range<const_iterator> types_range() const {
        return make_range<const_iterator>(types_begin(), types_end());
    };

    size_t types_size() const { return types.size(); }
    bool types_empty() const { return types.empty(); }

    void print(std::ostream &os) const;
    friend std::ostream &operator<<(std::ostream &os,
                                    const DataDeclaration &decl);
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
    AtomInt(int val) : val(val), Atom(Atom::AK_Int) {}
    void print(std::ostream &os) const;
    static bool classof(const Atom *S) { return S->getKind() == Atom::AK_Int; }
    int getVal() const { return val; }
};

class AtomIdent : public Atom {
    Identifier ident;

   public:
    AtomIdent(std::string ident) : ident(ident), Atom(Atom::AK_Ident) {}
    void print(std::ostream &os) const;

    Identifier getIdent() const { return ident; };

    static bool classof(const Atom *S) {
        return S->getKind() == Atom::AK_Ident;
    }
};

// *** Expression ****
class Expression {
   public:
    enum ExpressionKind { EK_Ap, EK_Cons, EK_Case };
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
    using iterator = ParamsTy::iterator;
    using const_iterator = ParamsTy::const_iterator;

    iterator begin() { return args.begin(); }
    iterator end() { return args.end(); }

    const_iterator begin() const { return args.begin(); }
    const_iterator end() const { return args.end(); }

    ExpressionAp(Identifier fn, std::initializer_list<Atom *> args)
        : fn(fn), args(args), Expression(Expression::EK_Ap){};

    ExpressionAp(Identifier fn, ArrayRef<Atom *> argsref)
        : fn(fn), Expression(Expression::EK_Ap) {
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
        : name(name), args(args), Expression(Expression::EK_Cons){};

    ExpressionConstructor(ConstructorName name, ArrayRef<Atom *> argsref)
        : name(name), Expression(Expression::EK_Cons) {
        for (Atom *arg : argsref) args.push_back(arg);
    };

    ConstructorName getName() const { return name; };
    void print(std::ostream &os) const;
    static bool classof(const Expression *E) {
        return E->getKind() == Expression::EK_Cons;
    }
};

// *** Alt ***
class CaseAlt {
   public:
    Expression *getRHS() { return rhs; }

    friend std::ostream &operator<<(std::ostream &os, const CaseAlt &a);
    virtual void print(std::ostream &os) const = 0;

    enum CaseAltKind { CAK_Int, CAK_Variable, CAK_Destructure };
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
    CaseAltDestructure(ConstructorName constructorName, ArrayRef<Identifier> varsref, Expression *rhs) : CaseAlt(CAK_Destructure, rhs), constructorName(constructorName) {
        for(Identifier var : varsref)
            vars.push_back(var);
    }

    const_iterator begin() const { return vars.begin(); } 
    const_iterator end() const { return vars.end(); }
    iterator_range<const_iterator> variables_range() const {
        return make_range(begin(), end());
    }
    static bool classof(const CaseAlt *a) {
        return a->getKind() == CaseAlt::CAK_Destructure;
    }
    void print(std::ostream &os) const;
};

// *** Case *** //
class ExpressionCase : public Expression {
   public:
    using AltsList = SmallVector<CaseAlt *, 2>;

   private:
    Atom *scrutinee;
    AltsList alts;

   public:
    ExpressionCase(Atom *scrutinee, ArrayRef<CaseAlt *> altsref)
        : scrutinee(scrutinee), Expression(Expression::EK_Case) {
        for (CaseAlt *alt : altsref) {
            alts.push_back(alt);
        }
    }

    void print(std::ostream &os) const;
    static bool classof(const Expression *E) {
        return E->getKind() == Expression::EK_Case;
    }
    Atom *getScrutinee() { return scrutinee; }
    const Atom *getScrutinee() const { return scrutinee; }

    using iterator = AltsList::iterator;
    using const_iterator = AltsList::const_iterator;

    const_iterator alts_begin() const { return alts.begin(); }
    const_iterator alts_end() const { return alts.end(); }
    size_t alts_size() const { return alts.size(); }

    iterator_range<const_iterator> alts_range() const {
        return make_range(alts_begin(), alts_end());
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
};

// *** Lambda ***
class Lambda {
   public:
    using ParamList = SmallVector<Parameter *, 4>;
    TypeName returnType;

   private:
    ParamList params;
    Expression *expr;

   public:
    Lambda(ArrayRef<Parameter *> paramsref, TypeName returnType,
           Expression *expr)
        : expr(expr), returnType(returnType) {
        for (Parameter *p : paramsref) {
            params.push_back(p);
        }
    }
    void print(std::ostream &os) const;
    friend std::ostream &operator<<(std::ostream &os, const Lambda &l);

    const Expression *getRhs() const { return expr; }

    using iterator = ParamList::iterator;
    using const_iterator = ParamList::const_iterator;

    iterator begin() { return params.begin(); }
    iterator end() { return params.end(); }

    const_iterator begin() const { return params.begin(); }
    const_iterator end() const { return params.end(); }

    size_t size() const { return params.size(); }
    bool empty() const { return params.empty(); }

    const Parameter *front() const { return params.front(); }
    Parameter *front() { return params.front(); }

    const Parameter *back() const { return params.back(); }
    Parameter *back() { return params.back(); }
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
    Program(ArrayRef<Binding *> bs, ArrayRef<DataDeclaration *> ds) {
        for (Binding *b : bs) bindings.push_back(b);
        for (DataDeclaration *d : ds) declarations.push_back(d);
    };

    using DataDeclarationList = SmallVector<DataDeclaration *, 4>;
    using BindingList = SmallVector<Binding *, 4>;

    using binding_iterator = BindingList::iterator;
    using declaration_iterator = DataDeclarationList::iterator;

    friend std::ostream &operator<<(std::ostream &os, const Program &p);

    binding_iterator bindings_begin() { return bindings.begin(); }
    binding_iterator bindings_end() { return bindings.end(); }

    iterator_range<binding_iterator> bindings_range() {
        return make_range(bindings_begin(), bindings_end());
    }

    declaration_iterator declarations_begin() { return declarations.begin(); }
    declaration_iterator declarations_end() { return declarations.end(); }

    iterator_range<declaration_iterator> declarations_range() {
        return make_range(declarations_begin(), declarations_end());
    }

   private:
    BindingList bindings;
    DataDeclarationList declarations;
};

}  // end namespace stg.
