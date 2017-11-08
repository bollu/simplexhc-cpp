#include "sxhc/stgir.h"

using namespace stg;

template<typename T>
void printSepBy(std::ostream &os, std::string prefix, ArrayRef<T *> ts, std::string separator, std::string postfix) {
  os << prefix;

  for(int i = 0; i < ts.size(); i++) {
    os << *ts[i];
    if (i <= (int)ts.size() - 2) {
      os << separator;
    }

  }
  os << postfix;
}

template<typename T>
void printSepBy(std::ostream &os, std::string prefix, ArrayRef<T> ts, std::string separator, std::string postfix) {
  os << prefix;

  for(int i = 0; i < ts.size(); i++) {
    os << ts[i];
    if (i <= (int)ts.size() - 2) {
      os << separator;
    }

  }
  os << postfix;
}
// DataConstructor
void DataConstructor::print(std::ostream &os) const {
  os << name;
  printSepBy(os, "(", ArrayRef<TypeName *>(types), " ", ")");
};

std::ostream &stg::operator <<(std::ostream &os, const DataConstructor &decl) {
  decl.print(os);
  return os;
}

// DataType
void DataType::print(std::ostream &os) const {
  os << "data ";
  os << name;
  os << " = ";
  printSepBy(os, "", ArrayRef<DataConstructor *>(constructors), "|", "");
};

std::ostream &stg::operator <<(std::ostream &os, const DataType &decl) {
  decl.print(os);
  return os;
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

std::string getIndentStr(const int nest, const int nlines) {
    static const int NUM_GUIDES=6;
    std::string indents[NUM_GUIDES] = { "┊","|","▏"," ▎", "▍", "▌" };
    std::string out;
    for(int i = 0; i < nest; i++) {
        out += indents[std::min<int>(i / 4, NUM_GUIDES)];
        out += "     ";
    }
    return out;
}

std::string indentedNewline(const int nest) {
    static int nlines = 0;
    nlines++;
    return "\n" + getIndentStr(nest, nlines);
}

void Lambda::print(std::ostream &os, int nest) const {
  os << "\\";
  if (freeparams.size() > 0)
      printSepBy(os, "(", (ArrayRef<Parameter *>)freeparams, " ", ") ");

  printSepBy(os, "(", (ArrayRef<Parameter *>)boundparams, " ", ")");
  os << " -> ";
  os << returnType << " ";
  os << "{"; os << indentedNewline(nest + 1);
  expr->print(os, nest+1);
  os << indentedNewline(nest); os << "}";
}

// Expression
void ExpressionAp::print(std::ostream &os, int nest) const {
  os << fn;
 printSepBy(os, "(", (ArrayRef<Atom *>)args, " ", ")");
};

void ExpressionConstructor::print(std::ostream &os, int nest) const {
    os << name;
    printSepBy(os, "(", ArrayRef<Atom*>(args), " ", ")");

}
void ExpressionCase::print(std::ostream &os, int nest) const {
    os << "(case";
    os << indentedNewline(nest + 1);
    scrutinee->print(os, nest + 1);
    os << indentedNewline(nest);
    os << "of";
    int i = 0;
    for(const CaseAlt *alt: alts) {
        os << indentedNewline(nest + 1);
        alt->print(os, nest + 1);

        i++;
    }
    os << indentedNewline(nest);
    os << ")";
}

void ExpressionLet::print(std::ostream &os, int nest) const {
    os << "(let";

    for (const Binding *b : bindings) {
        os << indentedNewline(nest + 1);
        b->print(os, nest + 1);
    }
    os << indentedNewline(nest);
    os << "in";
    os << indentedNewline(nest + 1);
    os << *rhs;
    os << indentedNewline(nest);
    os << ")";
}

void ExpressionIntLiteral::print(std::ostream &os, int nest) const {
    os << value;
}
// Case

std::ostream &stg::operator<<(std::ostream &os, const CaseAlt &a) {
    a.print(os);
    return os;
};
void CaseAltInt::print(std::ostream &os, int nest) const {
    os << "(casealt " << *lhs << " -> ";
    os << indentedNewline(nest + 1);
    rhs->print(os, nest + 1);
    os << indentedNewline(nest);
    os << ")";
}


void CaseAltVariable::print(std::ostream &os, int nest) const {
    os << "(caesalt " << lhs << " -> ";
    os << indentedNewline(nest + 1);
    rhs->print(os, nest + 1);
    os << indentedNewline(nest);
    os  << ")";
}

void CaseAltDestructure::print(std::ostream &os, int nest) const {
    os << "(casealt ";
    os << constructorName;
    printSepBy(os, "(", (ArrayRef<Identifier>)vars, " ", ")");
    os  << " -> ";
    os << indentedNewline(nest + 1);
    rhs->print(os, nest + 1);
    os << indentedNewline(nest);
    os << ")";
}


void CaseAltDefault::print(std::ostream &os, int nest) const {
    os << "(casealt default" << "->";
    os << indentedNewline(nest+1);
    rhs->print(os, nest + 1);
    os << indentedNewline(nest);
    os << ")";
}

// Binding
 std::ostream &stg::operator<<(std::ostream &os, const Binding &b) {
    b.print(os);
    return os;
}

void Binding::print(std::ostream &os, int nest) const {
    os << "(define " << this->lhs << " = ";
    this->rhs->print(os, nest + 1);
    os << indentedNewline(nest);
    os << ")";
}

// Program

std::ostream &stg::operator<<(std::ostream &os, const Program &p) {
    os << "\n";
    for (DataType *dt : p.datatypes){
      os << *dt << "\n";
    }
    for(Binding *b : p.bindings) {
        os << *b;
        os << "\n";
    }
    return os;
}
