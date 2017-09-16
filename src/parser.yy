%{
#include <math.h>
#include <iostream>
#include "parser.generated.hpp"
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
}

%start	stmt

%token	<ival>	INT
%token	<sval>	STRING
%%
stmt:
    INT stmt { cout << "Parser found int: " << $1 << endl; }
    | STRING stmt { cout << "Parser found string: " << *$1 << endl; }
    | INT { cout << "Parser found int (terminal): " << $1 << endl; }
    | STRING { cout << "Parser found string (terminal): " << *$1 << endl; }
    ;

%%

