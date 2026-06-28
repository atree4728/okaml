open Syntax

type t =
  | VInt of int
  | VBool of bool
  | VPair of t * t
  | VList of t list
  | VFun of name * expr * t Env.t
  | VRecFun of int * (name * expr) list * t Env.t
  | VCont of (t -> (t, Error.t) result)

let rec string_of_value = function
  | VInt i -> string_of_int i
  | VBool b -> string_of_bool b
  | VPair (a, b) -> Printf.sprintf "(%s, %s)" (string_of_value a) (string_of_value b)
  | VList l -> Printf.sprintf "[%s]" (List.map string_of_value l |> String.concat "; ")
  | VFun _ | VRecFun _ -> "<fun>"
  | VCont _ -> "<cont>"
;;

let tag_of_value = function
  | VInt _ -> "int"
  | VBool _ -> "bool"
  | VPair _ -> "pair"
  | VList _ -> "list"
  | VFun _ -> "fun"
  | VRecFun _ -> "recfun"
  | VCont _ -> "vcont"
;;

let expect_int = function
  | VInt n -> Ok n
  | v -> Error (Error.UnexpectedType (tag_of_value v, "int"))
;;

let expect_bool = function
  | VBool b -> Ok b
  | v -> Error (Error.UnexpectedType (tag_of_value v, "bool"))
;;

let expect_list = function
  | VList l -> Ok l
  | v -> Error (Error.UnexpectedType (tag_of_value v, "list"))
;;

let expect_fun = function
  | VFun (arg, expr, oenv) -> Ok (arg, expr, oenv)
  | v -> Error (Error.UnexpectedType (tag_of_value v, "fun"))
;;
