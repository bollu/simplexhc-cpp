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

extern int g_lexer_success;
extern int g_lexer_line_num;

void yyerror(const char *s) {
  fprintf(stderr, "line %d: %s\n", g_lexer_line_num, s);
  g_lexer_success = 0;
}

std::vector<Atom *> g_atoms;
std::vector<CaseAlt *> g_alts;
std::vector<Binding *> g_bindings;
std::vector<DataType *> g_datatypes;
std::vector<Parameter *> g_params;
std::vector<std::string *>g_types;
std::vector<DataConstructor *>g_dataconstructors;
stg::Program *g_program;

std::string *g_datadeclaration_name;
std::vector<Identifier> g_alt_destructure_vars;


void add_atom_to_list (Atom *a) {
  g_atoms.push_back(a);
}

void add_alt_to_list(CaseAlt *a) {
  g_alts.push_back(a);
}

void add_param_to_list(Parameter *p) {
  g_params.push_back(p);
}

void add_data_constructor_to_list(DataConstructor *b) {
  g_dataconstructors.push_back(b);
}
%}

%union{
  stg::Atom *atom;
  stg::CaseAlt *alt;
  stg::Expression *expr;
  stg::Lambda *lambda;
  stg::Binding *binding;
  stg::DataType *datatype;
  stg::Parameter *param;
  stg::DataConstructor *dataconstructor;
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
%token PIPE
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

%type <program> program
%type <binding> binding
%type <lambda> lambda
%type <expr> expr
%type <atom> atom
%type <datatype> datatype
%type <dataconstructor> dataconstructor

%type <UNDEF> atoms_;
%type <UNDEF> altlist;
%type <alt> alt;
%type <alt> altint;
%type <alt> altvar;
%type <alt> altdestructure;

%type <UNDEF> atomlist;

%type <UNDEF> params;
%type <param> param;

%type <UNDEF> topleveldefn;

%%
toplevel:
        program {
                  g_program = new stg::Program(g_bindings, g_datatypes); }

binding:
  BINDING ATOMSTRING ASSIGN lambda SEMICOLON { $$ = new stg::Binding(cast<AtomIdent>($2)->getIdent(), $4); };



// Data declaration
typeslist_:
  typeslist_ CONSTRUCTORNAME { g_types.push_back($2); }
  | CONSTRUCTORNAME { g_types.push_back($1); }
typeslist: OPENPAREN CLOSEPAREN | OPENPAREN typeslist_ CLOSEPAREN


dataconstructor:
  CONSTRUCTORNAME typeslist { 
    $$ = new stg::DataConstructor(*$1, g_types);
    g_types.clear();
  }

dataconstructorlist: 
                dataconstructorlist PIPE dataconstructor 
           {
                add_data_constructor_to_list($3);
           }
           | dataconstructor {
                add_data_constructor_to_list($1);
            }
datatype: DATA CONSTRUCTORNAME { g_datadeclaration_name = $2; } ASSIGN dataconstructorlist SEMICOLON {
               $$ = new stg::DataType(*g_datadeclaration_name, g_dataconstructors);
               g_dataconstructors.clear();
               delete g_datadeclaration_name;
               }

topleveldefn:
  binding { g_bindings.push_back($1); }
  | datatype { g_datatypes.push_back($1); }

program:
  program topleveldefn
  | topleveldefn

atom: 
  ATOMINT | ATOMSTRING

atoms_: 
  atoms_ atom { add_atom_to_list($2); }
  | atom { add_atom_to_list ($1); }

atomlist: OPENPAREN atoms_ CLOSEPAREN | OPENPAREN CLOSEPAREN

// Alternates
altlist: altlist alt SEMICOLON { add_alt_to_list($2); }
         | alt SEMICOLON { add_alt_to_list($1); }

alt: 
   altint | altvar | altdestructure

altint: 
      ATOMINT THINARROW expr { $$ = new stg::CaseAltInt(cast<AtomInt>($1), $3); }

altvar:
      ATOMSTRING THINARROW expr { $$ = new stg::CaseAltVariable(cast<AtomIdent>($1)->getIdent(), $3); }

// Note that we need a mid rule action so that we reserve the atoms
// consumed so far. Otherwise, expr will take all the atoms.
// case * of (Int (x y) -> print (z))
// would be parsed as:
// case * of (Int () -> print (x y z)
altdestructure: 
    CONSTRUCTORNAME atomlist {
     for(Atom *a : g_atoms) {
        g_alt_destructure_vars.push_back(cast<AtomIdent>(a)->getIdent());
     }
     g_atoms.clear();

    } THINARROW expr {
      // NOTE: the middle block is considered as a "marker". So, it's $3, NOT 
      // THINARROW.
      // Wish the docs were clearer about this.
      $$ = new stg::CaseAltDestructure(*$1, g_alt_destructure_vars, $5);
      g_alt_destructure_vars.clear();
      g_atoms.clear();
      delete $1;
    }

// Parameters
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
                        g_atoms.clear();

                        cout << "Ap: " << *$$ << "\n";
                        }
  | CONSTRUCTORNAME atomlist { 
    $$ = new stg::ExpressionConstructor(*$1, g_atoms);
    g_atoms.clear();
    delete $1;
  }
  | CASE atom OF altlist { $$ = new stg::ExpressionCase($2, g_alts); 
                                 g_alts.clear();}

%%

