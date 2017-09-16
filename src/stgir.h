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
  enum AtomKind { AK_Int, AK_Identifier };

  AtomKind getKind() const { return kind; }

protected:
  Atom(AtomKind kind) : kind(kind){};

private:
  const AtomKind kind;
};

class AtomInt : public Atom {
  int val;

public:
  AtomInt(int val) : val(val), Atom(Atom::AK_Int) {}

  static bool classof(const Atom *S) { return S->getKind() == Atom::AK_Int; }

  friend std::ostream &operator<<(std::ostream &os, const AtomInt &a);
};

class AtomIdentifier : public Atom {
  Identifier ident;

public:
  AtomIdentifier(std::string ident) : ident(ident), Atom(Atom::AK_Identifier) {}

  static bool classof(const Atom *S) {
    return S->getKind() == Atom::AK_Identifier;
  }
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
  SmallVector<Atom, 2> args;

public:
  ExpressionAp(Identifier fn, std::initializer_list<Atom> args)
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