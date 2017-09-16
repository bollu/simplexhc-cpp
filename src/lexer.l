/* scanner for a toy Pascal-like language */

%{
/* need this for the call to atof() below */
#pragma clang diagnostic ignored "-Wdeprecated-register"
#include <math.h>
#include <iostream>
using namespace std;
#include <stdio.h>
#include "parser.generated.hpp"
extern "C" int yylex();
extern "C" int yyparse();

%}

DIGIT    [0-9]
ID       [a-z][a-z0-9]*

%%

[ \t\n]          ;
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
