#include "stgir.h"

using namespace stg;

template<typename T>
void printSepBy(std::ostream &os, std::string prefix, ArrayRef<T *> ts, std::string separator, std::string postfix) {
  os << prefix;

  for(int i = 0; i < ts.size(); i++) {
    os << *ts[i];
    if (i < (int)ts.size() - 2) {
      os << separator;
    }

  }
  os << postfix;
}


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

//Parameter
std::ostream &stg::operator<<(std::ostream &os, const Parameter &p) {
  p.print(os);
  return os;
}

void Parameter::print(std::ostream &os) const {
  os << "(" << name << " : " << type << ")";
}

// Lambda

std::ostream &stg::operator<<(std::ostream &os, const Lambda &l) {
  l.print(os);
  return os;
}

void Lambda::print(std::ostream &os) const {
  os << "\\";
  printSepBy(os, "(", (ArrayRef<Parameter *>)params, ",", ")");
  os << " -> ";
  os << returnType << " ";
  os << "{ ";
  os << *expr;
  os << " }";
}

// Expression
void ExpressionAp::print(std::ostream &os) const {
  os << fn;
 printSepBy(os, "(", (ArrayRef<Atom *>)args, ",", ")");
};

void ExpressionConstructor::print(std::ostream &os) const {
    os << name;
    printSepBy(os, "(", ArrayRef<Atom*>(args), ",", ")");

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
    os << "\n";
    for(Binding *b : p.bindings) {
        os << *b;
        os << "\n";
    }
    return os;
}
