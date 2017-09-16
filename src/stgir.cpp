#include "stgir.h"

using namespace stg;

std::ostream &stg::operator<<(std::ostream &os, const Atom &a) {
  a.print(os);
  return os;
}


void AtomInt::print(std::ostream &os) const {
  os << "atom-" << val;
};

void AtomIdent::print(std::ostream &os) const {
  os << "atom-" << ident;
};


std::ostream &stg::operator<<(std::ostream &os, const Binding &b) {
    os << b.lhs << "=" << *b.rhs;
    return os;
}


void ExpressionAp::print(std::ostream &os) const {
  os << fn;
  os << "(";
  for (const Atom *a : args) {
    os << *a << " ";
  }
  os << ")";
};
