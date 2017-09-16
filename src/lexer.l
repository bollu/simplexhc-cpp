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

int g_lexer_line_num = 1;
%}


%%
"="            return ASSIGN; 
"("            return OPENPAREN; 
")"            return CLOSEPAREN;


[ \t]          ;

[0-9]+           { cout << "Found an integer:" << yytext << endl;
                    yylval.atom= new AtomInt(atoi(yytext));
                    return ATOMINT;
                 }
[a-zA-Z0-9]+     { cout << "Found a string: " << yytext << endl;
                    yylval.atom = new AtomIdent(yytext);
                    return ATOMSTRING;
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
    return 0;
}
