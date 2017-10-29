%{
#pragma clang diagnostic ignored "-Wdeprecated-register"
#include <math.h>
#include <iostream>
#include <string>
#include <stdio.h>
#include "stgir.h"
#include "llvm/Support/CommandLine.h"
#include "cxxopts.hpp"
#include <cstdio>

static const char *VERSION_STRING = "0.1.0";

using namespace stg;

// Keep this at the end so it can see all the includes made here.
#include "parser.generated.hpp"


using namespace std;
extern "C" int yylex();
extern "C" int yyparse();

extern stg::Program *g_program;
int g_lexer_line_num = 1;
int g_lexer_success = 1;

int compile_program(stg::Program *p, cxxopts::Options &options);

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
        cxxopts::Options options(argv[0], " - command line options.");
        options.positional_help("[optional args]");
            options.add_options()
            ("input-filepath", "Input file path", cxxopts::value<std::string>())
            ("emit-llvm", "Emit LLVM IR instead of assembly.")
            ("emit-asm", "Emit assembly.")
            ("jit", "JIT compiles and runs the input file.")
            ("o, output", "Output file path", cxxopts::value<std::string>()->default_value("-"))
            ("version", "version information");
            ("positional-container", "Continer for positional args", cxxopts::value<std::vector<std::string>>())
            ;
    options.parse_positional(std::vector<std::string>({"input-filepath", "positional-container"}));
    options.parse(argc, argv);

    assert(!(options.count("emit-llvm") && options.count("emit-asm")) && "cannot emit both assembly and LLVM at the same time");
    if (options.count("version")) {
        printf("%s\n", VERSION_STRING);
        return 0;
    }

    assert(options.count("input-filepath") && "input filename not given!");
    std::string OPTION_INPUT_FILENAME = options["input-filepath"].as<std::string>();

    yyin = fopen(OPTION_INPUT_FILENAME.c_str(), "r");
    if (yyin == NULL) {
        printf("unable to open input file: %s\n", OPTION_INPUT_FILENAME.c_str());
        perror("file open error: ");
        return 1;
    }

    do {
        yyparse();
    } while (!feof(yyin));
    if (g_lexer_success) {
        return compile_program(g_program, options);
    } else {
        return 1;
    }
}
