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
  int ival;
  std::string *sval;
  stg::Atom *atom;
}

%start	stmt

%token	<ival>	INT
%token	<sval>	STRING
%token END ENDL

%%
stmt:
    stmt INT { cout << "Parser found int: " << $2 << endl; }
    | stmt STRING { cout << "Parser found string: " << *$2 << endl; }
    | INT { cout << "Parser found int (terminal): " << $1 << endl; }
    | STRING { cout << "Parser found string (terminal): " << *$1 << endl; }
    | stmt ENDL { cout << "newline" << endl; }
    ;

%%

