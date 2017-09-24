#include "stgir.h"

using namespace stg;


// Atom
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




// Expression
void ExpressionAp::print(std::ostream &os) const {
  os << fn;
  os << "(";
  for (const Atom *a : args) {
    os << *a << " ";
  }
  os << ")";
};

void ExpressionConstructor::print(std::ostream &os) const {
    os << name;
  os << "(";
  for (const Atom *a : args) {
    os << *a << " ";
  }
  os << ")";

}
void ExpressionCase::print(std::ostream &os) const {
    os << "case (" << *scrutinee << ") of" << "\n";
    for(const CaseAlt *alt: alts) {
        os << "\t" << *alt << "\n";
    }
    os << "endcase\n";
}

// Case

std::ostream &stg::operator<<(std::ostream &os, const CaseAlt &a) {
    a.print(os);
    return os;
};
void CaseAltInt::print(std::ostream &os) const {
    os << *lhs << " -> " << *rhs << "\n";
}

// Binding
 std::ostream &stg::operator<<(std::ostream &os, const Binding &b) {
    os << "define " << b.lhs << " = " << *b.rhs;
    return os;
}

// Program

std::ostream &stg::operator<<(std::ostream &os, const Program &p) {
    for(Binding *b : p.bindings) {
        os << *b;
    }
    return os;
}
