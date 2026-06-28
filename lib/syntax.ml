type name = Name of string

type pattern =
  | PInt of int
  | PBool of bool
  | PVar of name
  | PPair of pattern * pattern
  | PNil
  | PCons of pattern * pattern

type expr =
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
  | EShift of name * expr
  | EReset of expr

type command =
  | CExp of expr
  | CDecl of (name * expr) list
  | CDeclRec of (name * expr) list

let string_of_name (Name name) = name

let rec string_of_pattern = function
  | PInt i -> Printf.sprintf "PInt(%d)" i
  | PBool b -> Printf.sprintf "PInt(%b)" b
  | PVar name -> Printf.sprintf "PVar(%s)" (string_of_name name)
  | PPair (p1, p2) ->
    Printf.sprintf "PPair(%s, %s)" (string_of_pattern p1) (string_of_pattern p2)
  | PNil -> "PNil"
  | PCons (p1, p2) ->
    Printf.sprintf "PCons(%s, %s)" (string_of_pattern p1) (string_of_pattern p2)
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
  | EShift (cont_name, expr) ->
    Printf.sprintf "EShift(%s, %s)" (string_of_name cont_name) (string_of_expr expr)
  | EReset expr -> Printf.sprintf "EReset(%s)" (string_of_expr expr)
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
