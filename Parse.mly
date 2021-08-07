%token FN LPAREN RPAREN EOF
%token <string> ERROR
%token <int> INT
%token <string> IDENT

%{
open Types
%}

%type <ex> prog
%start prog
%%

base
: x=INT { Int x }
| id=IDENT { Var id }
| LPAREN ex=expr RPAREN { ex }
| msg=ERROR { Error msg }

app
: f=app a=base { App (f, a) }
| ex=base { ex }

expr
: FN x=IDENT ex=expr { Fn (x, ex) }
| ex=app { ex }


prog
: ex=expr EOF { ex }

