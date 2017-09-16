#include "stgir.h"

using namespace stg;

std::ostream &stg::operator<<(std::ostream &os, const AtomInt &a) {
  os << "atom-" << a.val;
  return os;
};
