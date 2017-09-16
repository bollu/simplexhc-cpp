#pragma once
#include <iostream>
#include <llvm/ADT/SmallVector.h>
#include <llvm/Support/Casting.h>

namespace stg {

using namespace llvm;
using Identifier = std::string;

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
  void print(std::ostream &os) const ;
  static bool classof(const Atom *S) { return S->getKind() == Atom::AK_Int; }
};

class AtomIdent : public Atom {
  Identifier ident;

public:
  AtomIdent(std::string ident) : ident(ident), Atom(Atom::AK_Ident) {}
  void print(std::ostream &os) const;

  static bool classof(const Atom *S) { return S->getKind() == Atom::AK_Ident; }
};

// *** Expression ****
class Expression {
public:
  enum ExpressionKind { EK_Ap };

protected:
  Expression(ExpressionKind kind) : kind(kind){};

private:
  const ExpressionKind kind;
};

class ExpressionAp : public Expression {
  Identifier fn;
  SmallVector<Atom *, 2> args;

public:
  ExpressionAp(Identifier fn, std::initializer_list<Atom *> args)
      : fn(fn), args(args), Expression(Expression::EK_Ap){};
};

// *** Binding ***
class Binding {
  Identifier lhs;
  Expression rhs;
};

// *** Program ***
class Program {
  SmallVector<Binding, 4> bindings;
};

} // end namespace stg.