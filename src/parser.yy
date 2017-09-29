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
std::vector<DataDeclaration *> g_datadeclarations;
std::vector<Parameter *> g_params;
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

void add_param_to_list(Parameter *p) {
  g_params.push_back(p);
}
%}

%union{
  stg::Atom *atom;
  stg::CaseAlt *alt;
  stg::Expression *expr;
  stg::Lambda *lambda;
  stg::Binding *binding;
  stg::DataDeclaration *datadeclaration;
  stg::Parameter *param;
  std::string *constructorName;

  bool UNDEF;
}

%token ASSIGN
%token OPENPAREN
%token CLOSEPAREN
%token CASE
%token OF
%token SEMICOLON
%token COLON
%token THINARROW
%token EOFTOKEN
%token LAMBDA
%token OPENFLOWER
%token CLOSEFLOWER
%token BINDING
%token DATA

%start toplevel
%token <atom>	ATOMINT
%token <constructorName> CONSTRUCTORNAME
%token <atom>	ATOMSTRING
%token END ENDL

%type <program> program
%type <binding> binding
%type <lambda> lambda
%type <expr> expr
%type <atom> atom
%type <datadeclaration> datadeclaration

%type <UNDEF> atoms_;
%type <UNDEF> altlist;
%type <alt> alt;
%type <alt> altint;

%type <UNDEF> atomlist;

%type <UNDEF> params;
%type <param> param;

%type <UNDEF> topleveldefn;

%%
toplevel:
        program { g_program = new stg::Program(g_bindings, g_datadeclarations); }

binding:
  BINDING ATOMSTRING ASSIGN lambda { $$ = new stg::Binding(cast<AtomIdent>($2)->getIdent(), $4); };

datadeclaration:
  DATA CONSTRUCTORNAME atomlist { 
    std::vector<TypeName> types;
    for(Atom *a : g_atoms) { types.push_back(cast<AtomIdent>(a)->getIdent()); }
    $$ = new stg::DataDeclaration(*$2, types);
    g_atoms.clear();
  }

topleveldefn:
  binding { g_bindings.push_back($1); }
  | datadeclaration { g_datadeclarations.push_back($1); }

program:
  program topleveldefn lines 
  | topleveldefn lines

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

param:
  ATOMSTRING COLON CONSTRUCTORNAME { $$ = new stg::Parameter(cast<AtomIdent>($1)->getIdent(), *$3)}

params_:
  params_ param { add_param_to_list($2); }
  | param { add_param_to_list($1); }

params: 
  OPENPAREN params_ CLOSEPAREN | OPENPAREN CLOSEPAREN

lambda:
  LAMBDA params THINARROW CONSTRUCTORNAME 
    OPENFLOWER expr CLOSEFLOWER 
      { 
        $$ = new stg::Lambda(g_params, *$4, $6);
        g_params.clear(); 
      }

expr:
  // function application
  ATOMSTRING atomlist { $$ = new stg::ExpressionAp(cast<AtomIdent>($1)->getIdent(), g_atoms);
                        g_atoms.clear();}
  | CONSTRUCTORNAME atomlist { 
    $$ = new stg::ExpressionConstructor(*$1, g_atoms);
    g_atoms.clear();
    delete $1;
  }
  | CASE atom OF lines altlist { $$ = new stg::ExpressionCase($2, g_alts); 
                                 g_alts.clear();}

lines:
  lines ENDL |  ENDL
%%

