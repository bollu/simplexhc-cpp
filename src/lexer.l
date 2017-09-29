%{
#pragma clang diagnostic ignored "-Wdeprecated-register"
#include <math.h>
#include <iostream>
#include <stdio.h>
#include "stgir.h"
using namespace stg;

// Keep this at the end so it can see all the includes made here.
#include "parser.generated.hpp"


using namespace std;
extern "C" int yylex();
extern "C" int yyparse();

extern stg::Program *g_program;
int g_lexer_line_num = 1;

void compile_program(stg::Program *p);
%}


%%
"="            return ASSIGN; 
"("            return OPENPAREN; 
")"            return CLOSEPAREN;
"{"            return OPENFLOWER;
"}"            return CLOSEFLOWER;
"of"           return OF;
"case"         return CASE;
";"            return SEMICOLON;
"->"           return THINARROW;
"\\"           return LAMBDA;
"binding"      return BINDING;
"data"         return DATA;

[ \t]          ;

[0-9]+           { cout << "Found an integer:" << yytext << endl;
                    yylval.atom= new AtomInt(atoi(yytext));
                    return ATOMINT;
                 }
[a-z][a-zA-Z0-9]*     { cout << "Found ident: " << yytext << endl;
                    yylval.atom = new AtomIdent(yytext);
                    return ATOMSTRING;
                 }
[A-Z][a-zA-Z0-9]*     { cout << "Found a constructor name: " << yytext << endl;
                    yylval.constructorName = new std::string(yytext);
                    return CONSTRUCTORNAME;
                 }

\n             { ++g_lexer_line_num; return ENDL; }


%%

int yywrap(void) {
    return 1;
}

int main(void) {
    do {
        yyparse();
    } while (!feof(yyin));
    compile_program(g_program);
    return 0;
}
