%{
open Syntax

let rec to_curried_fun (args : name list) (expr : expr) =
  match args with
  | [] -> expr
  | arg :: rest -> EFun (arg, to_curried_fun rest expr)
;;
%}

%token <int>    INT
%token <bool>   BOOL
%token <string> ID
%token LET REC AND IN
%token PLUS TIMES MINUS DIV
%token EQ LT
%token LOGAND LOGOR
%token IF THEN ELSE
%token LPAR RPAR
%token FUN
%token ARROW
%token LBRACKET RBRACKET CONS COMMA
%token MATCH WITH BAR
%token SEMI SEMISEMI
%token EOF

%start <command option> toplevel

(* https://ocaml.org/manual/5.4/expr.html *)

%nonassoc IN
%nonassoc ELSE
%nonassoc ARROW
%nonassoc BAR
%right LOGOR
%right LOGAND
%left EQ LT
%right CONS
%left PLUS MINUS
%left TIMES DIV
%nonassoc UNARY
%%

toplevel:
  | expr SEMISEMI           { Some(CExp     $1) }
  | let_binding    SEMISEMI { Some(CDecl    $1) }
  | letrec_binding SEMISEMI { Some(CDeclRec $1) }
  | EOF                     { None }

id:
  | ID { Name $1 }

let_binding:
  | LET     id list(id) EQ expr list(and_binding) { ($2, to_curried_fun $3 $5) :: $6 }

letrec_binding:
  | LET REC id list(id) EQ expr list(and_binding) { ($3, to_curried_fun $4 $6) :: $7 }

and_binding:
  | AND     id list(id) EQ expr                   { ($2, to_curried_fun $3 $5) }

pattern:
  | pattern CONS pattern            { PCons($1, $3) }
  | INT                             { PInt($1)      }
  | BOOL                            { PBool($1)     }
  | id                              { PVar($1) }
  | LPAR pattern COMMA pattern RPAR { PPair($2, $4) }
  | LBRACKET RBRACKET               { PNil          }
  | LPAR pattern RPAR               { $2            }

cases:
  | pattern ARROW expr           { [($1, $3)]     }
  | pattern ARROW expr BAR cases { ($1, $3) :: $5 }

list_expr:
  | expr                { ECons($1, ENil) }
  | expr SEMI           { ECons($1, ENil) }
  | expr SEMI list_expr { ECons($1, $3) }

expr:
  | let_binding    IN expr      { ELet   ($1, $3) }
  | letrec_binding IN expr      { ELetRec($1, $3) }
  | IF expr THEN expr ELSE expr { EIf ($2, $4, $6) }
  | expr LOGOR  expr            { EOr  ($1, $3) }
  | expr LOGAND expr            { EAnd ($1, $3) }
  | expr EQ  expr               { EEq  ($1, $3) }
  | expr LT  expr               { ELt  ($1, $3) }
  | MATCH expr WITH cases       { EMatch($2, $4) }
  | MATCH expr WITH BAR cases   { EMatch($2, $5) }
  | FUN list(id) ARROW expr     { to_curried_fun $2 $4 }
  | expr CONS  expr             { ECons($1, $3) }
  | expr PLUS  expr             { EAdd ($1, $3) }
  | expr MINUS expr             { ESub ($1, $3) }
  | expr TIMES expr             { EMul ($1, $3) }
  | expr DIV   expr             { EDiv ($1, $3) }
  | PLUS  expr %prec UNARY      { EAdd (EConstInt(0), $2) }
  | MINUS expr %prec UNARY      { ESub (EConstInt(0), $2) }
  | app_expr                    { $1 } 
  | atomic_expr                 { $1 }

app_expr:
  | app_expr    atomic_expr { EApp ($1, $2) }
  | atomic_expr atomic_expr { EApp ($1, $2) }

atomic_expr:
  | INT                         { EConstInt($1)  }
  | BOOL                        { EConstBool($1) }
  | LPAR expr COMMA expr RPAR   { EPair($2, $4)  }
  | LBRACKET RBRACKET           { ENil           }
  | LBRACKET list_expr RBRACKET { $2 }
  | id                          { EVar($1)  }
  | LPAR expr RPAR              { $2             }
