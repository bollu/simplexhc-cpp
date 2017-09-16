%{
#include <math.h>
#include <iostream>
#include "stgir.h"

using namespace std;

#define YYERROR_VERBOSE
using namespace std;

extern "C" int yylex();
extern "C" int yyparse();

void yyerror(const char *err) {
    std::cerr << "YYerr: " << err << "\n";
}
%}


%union{
  stg::Atom *atom;
}

%start	stmt

%token	<atom>	ATOMINT
%token	<atom>	ATOMSTRING
%token END ENDL

%%
stmt:
    stmt ATOMINT { cout << "Parser found ATOMINT: " << *$2 << endl; }
    | stmt ATOMSTRING { cout << "Parser found ATOMSTRING: " << *$2 << endl; }
    | ATOMINT { cout << "Parser found int (terminal): " << *$1 << endl; }
    | ATOMSTRING { cout << "Parser found string (terminal): " << *$1 << endl; }
    | stmt ENDL { cout << "newline" << endl; }
    ;


%%

