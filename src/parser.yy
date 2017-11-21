%{
#include <math.h>
#include <stack>
#include <iostream>
#include <vector>
#include "sxhc/stgir.h"

using namespace std;

#define YYERROR_VERBOSE
using namespace std;
using namespace stg; 
using namespace llvm;
//extern "C" int yylex(void);
//extern "C" int yyparse(void);
int yylex(void);
int yyparse(void);

extern int g_lexer_success;
extern int g_lexer_line_num;

using ParamList = Lambda::ParamList;

void yyerror(const char *s) {
  fprintf(stderr, "line %d: %s\n", g_lexer_line_num, s);
  g_lexer_success = 0;
}

std::vector<Binding *> g_toplevel_bindings;
std::vector<Binding *> g_let_bindings;
std::vector<DataType *> g_datatypes;
ParamList g_params;
std::vector<std::string *>g_types;
std::vector<DataConstructor *>g_dataconstructors;
stg::Program *g_program;


std::stack<ParamList> g_params_stack;


std::string *g_datadeclaration_name;
std::vector<Identifier> g_alt_destructure_vars;


ParamList g_lambda_freevars;



void add_param_to_list(Parameter *p) {
  g_params.push_back(p);
}

void add_data_constructor_to_list(DataConstructor *b) {
  g_dataconstructors.push_back(b);
}
%}

%union{
  std::vector<Atom *> *atomslist;
  std::vector<TypeName> *typeslist;
  std::vector<stg::CaseAlt*> *altslist;
  stg::Atom *atom;
  stg::CaseAlt *alt;
  stg::Expression *expr;
  stg::Lambda *lambda;
  stg::Binding *binding;
  stg::DataType *datatype;
  stg::Parameter *param;
  stg::DataConstructor *dataconstructor;
  std::string *constructorName;
  stg::TypeRaw *typeraw;

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
%token LET
%token IN
%token DEFAULT

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
%type <typeraw> typeraw

%type <altslist> altlist;
%type <alt> alt;
%type <alt> altint;
%type <alt> altvar;
%type <alt> altdefault;
%type <alt> altdestructure;

%type <atomslist> atoms_;
%type <atomslist> atomlist;


%type <typeslist> typeraw_typeslist_;
%type <typeslist> typeraw_typeslist;

%type <UNDEF> params;
%type <param> param;

%type <UNDEF> topleveldefn;

%%
toplevel:
        program {
                  g_program = new stg::Program(g_toplevel_bindings, g_datatypes); }

binding:
  BINDING ATOMSTRING ASSIGN lambda SEMICOLON { $$ = new stg::Binding(cast<AtomIdent>($2)->getIdent(), $4); };



// Data declaration
datacons_typeslist_:
  datacons_typeslist_ CONSTRUCTORNAME { g_types.push_back($2); }
  | CONSTRUCTORNAME { g_types.push_back($1); }
datacons_typeslist: OPENPAREN CLOSEPAREN | OPENPAREN datacons_typeslist_ CLOSEPAREN


dataconstructor:
  CONSTRUCTORNAME datacons_typeslist {
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
  binding { g_toplevel_bindings.push_back($1); }
  | datatype { g_datatypes.push_back($1); }

program:
  program topleveldefn
  | topleveldefn

atom: 
  ATOMINT | ATOMSTRING

atoms_: 
  atoms_ atom {
    $$ = $1;
    $$->push_back($2);
  }
  | atom {  $$ = new std::vector<Atom*>(); $$->push_back($1); }

atomlist: OPENPAREN atoms_ CLOSEPAREN {
  $$ = $2;
}
| OPENPAREN CLOSEPAREN {
    $$ = new std::vector<Atom*>();
}

// Alternates
altlist: altlist alt SEMICOLON {
    $1->push_back($2);
    $$ = $1;
} | alt SEMICOLON { $$ = new std::vector<stg::CaseAlt *>();
                   $$->push_back($1);
}


altint: 
      ATOMINT THINARROW expr { $$ = new stg::CaseAltInt(cast<AtomInt>($1), $3); }

altvar:
      ATOMSTRING THINARROW expr { $$ = new stg::CaseAltVariable(cast<AtomIdent>($1)->getIdent(), $3); }

altdefault: DEFAULT THINARROW expr { $$ = new stg::CaseAltDefault($3); }

alt: 
   altint | altvar | altdestructure | altdefault
// Note that we need a mid rule action so that we reserve the atoms
// consumed so far. Otherwise, expr will take all the atoms.
// case * of (Int (x y) -> print (z))
// would be parsed as:
// case * of (Int () -> print (x y z)
altdestructure: 
    CONSTRUCTORNAME atomlist THINARROW expr {
      // NOTE: the middle block is considered as a "marker". So, it's $3, NOT 
      // THINARROW.
      // Wish the docs were clearer about this.
      std::vector<Identifier> idents;
      for(Atom *a : *$2) {
          AtomIdent *ident = cast<AtomIdent>(a);
          idents.push_back(ident->getIdent());
      }
      $$ = new stg::CaseAltDestructure(*$1, idents, $4);
      g_alt_destructure_vars.clear();
      delete $1;
    }

// types: either CONSTRUCTOR | RETTY(TYPES )
typeraw: CONSTRUCTORNAME
    {
        $$ = new stg::DataTypeRaw(*$1);
    }
    |
    typeraw_typeslist THINARROW CONSTRUCTORNAME {
        $$ = new stg::FunctionTypeRaw(*$3, *$1);
    }
    |
    OPENPAREN CLOSEPAREN THINARROW CONSTRUCTORNAME {
        $$ = new stg::FunctionTypeRaw(*$4, {});
    }

typeraw_typeslist: OPENPAREN typeraw_typeslist_ {
    $$ = $2;
}

typeraw_typeslist_: CONSTRUCTORNAME typeraw_typeslist_  {
    $$ = $2;
    $$->push_back(*$1);
} | CONSTRUCTORNAME CLOSEPAREN {
    $$ = new std::vector<TypeName>();
    $$->push_back(*$1);
}

// Parameters
param:
  ATOMSTRING COLON typeraw { $$ = new stg::Parameter(cast<AtomIdent>($1)->getIdent(), $3); }

params_:
  params_ param { add_param_to_list($2); }
  | param { add_param_to_list($1); }

params: 
  OPENPAREN { g_params = {}; }
  params_ CLOSEPAREN {
      g_params_stack.push(g_params);
      g_params = {};
  } | OPENPAREN CLOSEPAREN {
    g_params_stack.push(ParamList());
    g_params = {};
  }

lambda:
  LAMBDA params THINARROW CONSTRUCTORNAME 
    OPENFLOWER expr CLOSEFLOWER 
      { 
        $$ = new stg::Lambda(g_params_stack.top(), *$4, $6);
        g_params_stack.pop();
      }
  |
  // syntax for free vars
  LAMBDA params params THINARROW CONSTRUCTORNAME OPENFLOWER expr CLOSEFLOWER {

    ParamList boundvars = g_params_stack.top();
    g_params_stack.pop();
    ParamList freevars = g_params_stack.top();
    g_params_stack.pop();

    $$ = new stg::Lambda(freevars, boundvars, *$5, $7);
    g_params.clear();
  }


letbindings: binding { g_let_bindings.push_back($1); } |
             letbindings binding { g_let_bindings.push_back($2); }

expr:
  // function application
  ATOMSTRING atomlist { $$ = new stg::ExpressionAp(cast<AtomIdent>($1)->getIdent(), *$2); }
  | CONSTRUCTORNAME atomlist { 
    $$ = new stg::ExpressionConstructor(*$1, *$2);
    delete $1;
  }
  | CASE expr OF altlist { $$ = new stg::ExpressionCase($2,  *$4); }
  | LET letbindings IN expr {
    $$ =  new stg::ExpressionLet(g_let_bindings, $4);
    g_let_bindings.clear();
  }

  | ATOMINT {
    $$ = new stg::ExpressionIntLiteral(cast<AtomInt>($1)->getVal());
  }

%%

