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
std::vector<CaseAlt *> g_alts;
std::vector<Binding *> g_bindings;
stg::Program *g_program;


void yyerror(const char *err) {
    std::cerr << "YYerr: " << err << "\n";
}

void add_atom_to_list (Atom *a) {
  g_atoms.push_back(a);
}

void add_alt_to_list(CaseAlt *a) {
  g_alts.push_back(a);
}
%}

%union{
  stg::Atom *atom;
  stg::CaseAlt *alt;
  stg::Expression *expr;
  stg::Binding *binding;
  std::string *constructorName;

  bool UNDEF;
}

%token ASSIGN
%token OPENPAREN
%token CLOSEPAREN
%token CASE
%token OF
%token SEMICOLON
%token THINARROW
%token EOFTOKEN

%start toplevel
%token <atom>	ATOMINT
%token <constructorName> CONSTRUCTORNAME
%token <atom>	ATOMSTRING
%token END ENDL

%type <program> program
%type <binding> binding
%type <expr> expr;
%type <atom> atom;
%type <alt> alt;
%type <alt> altint;
%type <UNDEF> atoms_;
%type <UNDEF> altlist;

%type <UNDEF> atomlist;


%%
toplevel:
        program { g_program = new stg::Program(g_bindings); }
program:
  program binding lines { g_bindings.push_back($2); }
  | binding lines { g_bindings.push_back($1); }

atom: 
  ATOMINT | ATOMSTRING

atoms_: 
  atoms_ atom { add_atom_to_list($2); }
  | atom { add_atom_to_list ($1); }

atomlist: OPENPAREN atoms_ CLOSEPAREN | OPENPAREN CLOSEPAREN

altlist: altlist alt { add_alt_to_list($2); }
         | alt SEMICOLON { add_alt_to_list($1); }
alt: 
   altint
   //altint | altconstructor | altdefault

altint: 
      ATOMINT THINARROW expr { $$ = new stg::CaseAltInt(cast<AtomInt>($1), $3); }

expr:
  // function application
  ATOMSTRING atomlist { $$ = new stg::ExpressionAp(cast<AtomIdent>($1)->getIdent(), g_atoms);
                        g_atoms.clear();}
  | CONSTRUCTORNAME atomlist { $$ = new stg::ExpressionConstructor(*$1, g_atoms);
                                delete $1;
                         }
  | CASE atom OF lines altlist { $$ = new stg::ExpressionCase($2, g_alts); 
                                 g_alts.clear();}

binding:
  ATOMSTRING ASSIGN expr { $$ = new stg::Binding(cast<AtomIdent>($1)->getIdent(), $3); };

lines:
  lines ENDL |  ENDL
%%

