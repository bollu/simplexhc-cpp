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
