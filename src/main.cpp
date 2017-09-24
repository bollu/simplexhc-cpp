#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/IRBuilder.h"
#include <iostream>
#include "stgir.h"

using namespace std;

// http://web.iitd.ac.in/~sumeet/flex__bison.pdf
// http://aquamentus.com/flex_bison.html
//

void compile_program(stg::Program *program) {
    cout << "> program: " << *program << "\n";
}
