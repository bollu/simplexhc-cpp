%{
#pragma clang diagnostic ignored "-Wdeprecated-register"
#include <math.h>
#include <iostream>
#include <string>
#include <stdio.h>
#include "stgir.h"
#include "llvm/Support/CommandLine.h"
using namespace stg;

// Keep this at the end so it can see all the includes made here.
#include "parser.generated.hpp"


using namespace std;
extern "C" int yylex();
extern "C" int yyparse();

extern stg::Program *g_program;
int g_lexer_line_num = 1;
int g_lexer_success = 1;

int compile_program(stg::Program *p, int argc, char **argv);

llvm::cl::opt<std::string> OPTION_INPUT_FILENAME(llvm::cl::Positional, llvm::cl::desc("<input filename>"), llvm::cl::Required);
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
":"            return COLON;
"->"           return THINARROW;
"|"            return PIPE;
"\\"           return LAMBDA;
"binding"      return BINDING;
"data"         return DATA;
"let"         return LET;
"in"         return IN;
"default"         return DEFAULT;

[ \t]          ;

[0-9]+           { 
                    yylval.atom= new AtomInt(atoi(yytext));
                    return ATOMINT;
                 }
[a-z_][a-zA-Z0-9_]*     {
                    yylval.atom = new AtomIdent(yytext);
                    return ATOMSTRING;
                 }
[A-Z][a-zA-Z0-9_]*     {
                    yylval.constructorName = new std::string(yytext);
                    return CONSTRUCTORNAME;
                 }

#.*           { /* DO NOTHING */ }
\n             { ++g_lexer_line_num; }
%%

int yywrap(void) {
    return 1;
}

int main(int argc, char **argv) {
    cl::ParseCommandLineOptions(argc, argv);
    printf(__FILE__);
    printf("input filename: %s", OPTION_INPUT_FILENAME.c_str());
    yyin = fopen(OPTION_INPUT_FILENAME.c_str(), "r");
    printf(__FILE__);
    do {
        yyparse();
    } while (!feof(yyin));
    if (g_lexer_success) {
        return compile_program(g_program, argc, argv);
    } else {
        return 1;
    }
}
