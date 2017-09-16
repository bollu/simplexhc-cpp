%{
#include <math.h>
#include <iostream>
#include "stgir.h"

using namespace std;

#define YYERROR_VERBOSE
#include <vector>
using namespace std;
using namespace stg; 
using namespace llvm;
extern "C" int yylex();
extern "C" int yyparse();

std::vector<Atom *> g_atoms;


void yyerror(const char *err) {
    std::cerr << "YYerr: " << err << "\n";
}

void add_atom_to_list (Atom *a) {
  g_atoms.push_back(a);
}
%}

%union{
  stg::Atom *atom;
  stg::Expression *expr;
  stg::Binding *binding;
  stg::Program *program;

  bool UNDEF;
}

%token ASSIGN
%token OPENPAREN
%token CLOSEPAREN

%start program
%token <atom>	ATOMINT
%token <atom>	ATOMSTRING
%token END ENDL

%type <program> program
%type <binding> binding
%type <expr> expr;
%type <atom> atom;
%type <UNDEF> atoms_;

%type <atoms> atomlist;


%%
program:
  program binding lines { std::cout << "binding (nonterminal)\n" << *$2 << "\n"; }
  | binding lines { std::cout << "final (termina)\n"; }

atom: 
  ATOMINT | ATOMSTRING

atoms_: 
  atoms_ ATOMINT { add_atom_to_list($2); }
  | atoms_ ATOMSTRING { add_atom_to_list($2); }
  | ATOMINT { add_atom_to_list ($1); }
  | ATOMSTRING { add_atom_to_list($1); }

atomlist: OPENPAREN atoms_ CLOSEPAREN | OPENPAREN CLOSEPAREN

expr:
  // function application
  ATOMSTRING atomlist { $$ = new stg::ExpressionAp(cast<AtomIdent>($1)->getIdent(), g_atoms);
                        g_atoms.clear();};

binding:
  ATOMSTRING ASSIGN expr { $$ = new stg::Binding(cast<AtomIdent>($1)->getIdent(), $3); };

lines:
  lines ENDL | ENDL
%%

