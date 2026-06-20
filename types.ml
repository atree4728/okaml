type name = Name of string

type pattern =
  | PInt of int
  | PBool of bool
  | PVar of name
  | PPair of pattern * pattern
  | PNil
  | PCons of pattern * pattern

module M = Map.Make (String)

type 'a env = Env of 'a M.t

and value =
  | VInt of int
  | VBool of bool
  | VPair of value * value
  | VList of value list
  | VFun of name * expr * value env
  | VRecFun of int * (name * expr) list * value env

and expr =
  | EConstInt of int
  | EConstBool of bool
  | EVar of name
  | EAdd of expr * expr
  | ESub of expr * expr
  | EMul of expr * expr
  | EDiv of expr * expr
  | EEq of expr * expr
  | ELt of expr * expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | EIf of expr * expr * expr
  | ELet of (name * expr) list * expr
  | ELetRec of (name * expr) list * expr
  | ENil
  | ECons of expr * expr
  | EPair of expr * expr
  | EMatch of expr * (pattern * expr) list
  | EFun of name * expr
  | EApp of expr * expr

type command =
  | CExp of expr
  | CDecl of (name * expr) list
  | CDeclRec of (name * expr) list

type tyvar = Idx of int

type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
  | TyPair of ty * ty
  | TyList of ty

type type_schema = TySchema of tyvar list * ty

type evalError =
  | Unbound of string
  | UnexpectedType of string * string
  | DivisionByZero of string
  | MatchFailure of string
  | RecursiveType of string * string
  | LetRecForNonFunc

let string_of_name (Name name) = name

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VPair (a, b) -> Printf.sprintf "(%s, %s)" (string_of_value a) (string_of_value b)
  | VList l -> Printf.sprintf "[%s]" (List.map string_of_value l |> String.concat "; ")
  | VFun _ | VRecFun _ -> "<fun>"
;;

let rec string_of_pattern = function
  | PInt i -> string_of_int i
  | PBool b -> string_of_bool b
  | PVar name -> string_of_name name
  | PPair (p1, p2) ->
    Printf.sprintf "(%s, %s)" (string_of_pattern p1) (string_of_pattern p2)
  | PNil -> "[]"
  | PCons (p1, p2) ->
    Printf.sprintf "%s :: %s" (string_of_pattern p1) (string_of_pattern p2)
;;

let rec string_of_expr = function
  | EConstInt i -> string_of_int i
  | EConstBool b -> string_of_bool b
  | EVar x -> string_of_name x
  | EAdd (e1, e2) -> Printf.sprintf "EAdd(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | ESub (e1, e2) -> Printf.sprintf "ESub(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | EMul (e1, e2) -> Printf.sprintf "EMul(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | EDiv (e1, e2) -> Printf.sprintf "EDiv(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | EEq (e1, e2) -> Printf.sprintf "EEq(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | ELt (e1, e2) -> Printf.sprintf "ELt(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | EAnd (e1, e2) -> Printf.sprintf "EAnd(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | EOr (e1, e2) -> Printf.sprintf "EOr(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | EIf (e1, e2, e3) ->
    Printf.sprintf
      "EIf(%s, %s, %s)"
      (string_of_expr e1)
      (string_of_expr e2)
      (string_of_expr e3)
  | ELet (bindings, e2) ->
    Printf.sprintf
      "ELet([%s], %s)"
      (String.concat ", "
       @@ List.map
            (fun (name, expr) ->
               Printf.sprintf "(%s, %s)" (string_of_name name) (string_of_expr expr))
            bindings)
      (string_of_expr e2)
  | ELetRec (bindings, e2) ->
    Printf.sprintf
      "ELetRec([%s], %s)"
      (String.concat ", "
       @@ List.map
            (fun (name, expr) ->
               Printf.sprintf "(%s, %s)" (string_of_name name) (string_of_expr expr))
            bindings)
      (string_of_expr e2)
  | ENil -> "ENil"
  | ECons (e1, e2) ->
    Printf.sprintf "ECons(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | EPair (e1, e2) ->
    Printf.sprintf "EPair(%s, %s)" (string_of_expr e1) (string_of_expr e2)
  | EMatch (e, pats) ->
    Printf.sprintf
      "EMatch(%s, [%s])"
      (string_of_expr e)
      (String.concat
         ", "
         (List.map
            (fun (p, e') ->
               Printf.sprintf "(%s, %s)" (string_of_pattern p) (string_of_expr e'))
            pats))
  | EFun (arg, e) -> Printf.sprintf "EFun(%s, %s)" (string_of_name arg) (string_of_expr e)
  | EApp (e1, e2) -> Printf.sprintf "EApp(%s, %s)" (string_of_expr e1) (string_of_expr e2)
;;

let string_of_command p =
  match p with
  | CExp e -> Printf.sprintf "CExp(%s)" (string_of_expr e)
  | CDecl decls ->
    List.map
      (fun (name, e) ->
         Printf.sprintf "CDecl(%s,  %s)" (string_of_name name) (string_of_expr e))
      decls
    |> String.concat "; "
  | CDeclRec decls ->
    List.map
      (fun (name, e) ->
         Printf.sprintf "CDeclRec(%s, %s)" (string_of_name name) (string_of_expr e))
      decls
    |> String.concat "; "
;;

let tag_of_value = function
  | VInt _ -> "int"
  | VBool _ -> "bool"
  | VPair _ -> "pair"
  | VList _ -> "list"
  | VFun _ -> "fun"
  | VRecFun _ -> "recfun"
;;

let string_of_tyvar (Idx i) = Printf.sprintf "'a%d" i

let rec string_of_type = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1, t2) -> Printf.sprintf "(%s -> %s)" (string_of_type t1) (string_of_type t2)
  | TyVar a -> string_of_tyvar a
  | TyPair (t1, t2) -> Printf.sprintf "(%s * %s)" (string_of_type t1) (string_of_type t2)
  | TyList t -> Printf.sprintf "(%s list)" (string_of_type t)
;;

let string_of_type_schema (TySchema (abs_tyvars, ty)) =
  Printf.sprintf
    "∀ %s. %s"
    (abs_tyvars |> List.map string_of_tyvar |> String.concat ", ")
    (string_of_type ty)
;;

let string_of_error = function
  | Unbound name -> "Error: Unbound value " ^ name
  | UnexpectedType (actual, expected) ->
    Printf.sprintf
      "Error: The value has type `%s` but an expression was expected of type `%s`"
      actual
      expected
  | DivisionByZero expr -> "Error: Division by zero: " ^ expr
  | MatchFailure expr -> "Error: Match failure: " ^ expr
  | RecursiveType (tyvar, ty) ->
    Printf.sprintf "Error: Detected a recursive type definition: %s = %s" tyvar ty
  | LetRecForNonFunc -> "Error: `let rec` is allowed only for function binding."
;;
