#pragma once
#include <llvm/ADT/ArrayRef.h>
#include <llvm/ADT/SmallVector.h>
#include <iostream>

#include <llvm/Support/Casting.h>

namespace stg {

using namespace llvm;
using Identifier = std::string;
using ConstructorName = std::string;

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
    ConstructorName name;
    SmallVector<Atom *, 2> args;

   public:
    ExpressionConstructor(ConstructorName name,
                          std::initializer_list<Atom *> args)
        : name(name), args(args), Expression(Expression::EK_Cons){};

    ExpressionConstructor(ConstructorName name, ArrayRef<Atom *> argsref)
        : name(name), Expression(Expression::EK_Cons) {
        for (Atom *arg : argsref) args.push_back(arg);
    };

    void print(std::ostream &os) const;
    static bool classof(const Expression *E) {
        return E->getKind() == Expression::EK_Cons;
    }
};

// *** Alt ***
class CaseAlt {
   protected:
    Expression *rhs;
    CaseAlt(Expression *rhs) : rhs(rhs){};

   public:
    Expression *getRHS() { return rhs; }

    friend std::ostream &operator<<(std::ostream &os, const CaseAlt &a);
    virtual void print(std::ostream &os) const = 0;
};

class CaseAltInt : public CaseAlt {
    AtomInt *lhs;

   public:
    CaseAltInt(AtomInt *lhs, Expression *rhs) : CaseAlt(rhs), lhs(lhs){};
    void print(std::ostream &os) const;
};

class ExpressionCase : public Expression {
    Atom *scrutinee;
    SmallVector<CaseAlt *, 2> alts;

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
};

// *** Binding ***
class Binding {
    Identifier lhs;
    Expression *rhs;

   public:
    Binding(Identifier lhs, Expression *rhs) : lhs(lhs), rhs(rhs){};
    friend std::ostream &operator<<(std::ostream &os, const Binding &b);
    Identifier getName() const { return lhs; }
    const Expression *getRhs() const { return rhs; }
};

// *** Program ***
class Program {
   public:
    Program(ArrayRef<Binding *> bs) {
        for (Binding *b : bs) bindings.push_back(b);
    };

    using BindingList = SmallVector<Binding *, 4>;
    using iterator = BindingList::iterator;

    friend std::ostream &operator<<(std::ostream &os, const Program &p);

    iterator begin() { return bindings.begin(); }
    iterator end() { return bindings.end(); }

    size_t size() const { return bindings.size(); }
    bool empty() const { return bindings.empty(); }

    const Binding *front() const { return bindings.front(); }
    Binding *front() { return bindings.front(); }

    const Binding *back() const { return bindings.back(); }
    Binding *back() { return bindings.back(); }

   private:
    BindingList bindings;
};

}  // end namespace stg.
