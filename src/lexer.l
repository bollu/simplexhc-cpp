%{
#pragma clang diagnostic ignored "-Wdeprecated-register"
#include <math.h>
#include <iostream>
#include <stdio.h>
#include "stgir.h"


// Keep this at the end so it can see all the includes made here.
#include "parser.generated.hpp"


using namespace std;
extern "C" int yylex();
extern "C" int yyparse();

int g_lexer_line_num = 1;
%}


%%

[ \t]          ;
[0-9]+\.[0-9]+   { cout << "Found a floating-point number:" << yytext << endl;
                    yylval.ival= atoi(yytext);
                    return INT;
                }
[0-9]+           { cout << "Found an integer:" << yytext << endl;
                    yylval.ival= atoi(yytext);
                    return INT;
                 }
[a-zA-Z0-9]+     { cout << "Found a string: " << yytext << endl;
                    yylval.sval= new std::string(yytext);
                    return STRING;
                 }
\n             { ++g_lexer_line_num; return ENDL; }
.                ;

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
