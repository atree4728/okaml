type tyvar = Idx of int

type t =
  | TyInt
  | TyBool
  | TyFun of t * t * t * t (* τ₁ / α → τ₂ / β *)
  | TyVar of tyvar
  | TyPair of t * t
  | TyList of t
  | TyToplevel

type schema = Schema of tyvar list * t

let schema_of ty = Schema ([], ty)
let tyvar_of n = Idx n
let string_of_tyvar (Idx i) = Printf.sprintf "'a%d" i

let rec string_of_type = function
  | TyInt -> "int"
  | TyBool -> "bool"
  | TyFun (t1, a, t2, b) ->
    Printf.sprintf
      "((%s / %s) -> (%s / %s))"
      (string_of_type t1)
      (string_of_type a)
      (string_of_type t2)
      (string_of_type b)
  | TyVar a -> string_of_tyvar a
  | TyPair (t1, t2) -> Printf.sprintf "(%s * %s)" (string_of_type t1) (string_of_type t2)
  | TyList t -> Printf.sprintf "(%s list)" (string_of_type t)
  | TyToplevel -> "toplevel"
;;

let string_of_type_schema (Schema (abs_tyvars, ty)) =
  Printf.sprintf
    "∀ %s. %s"
    (abs_tyvars |> List.map string_of_tyvar |> String.concat ", ")
    (string_of_type ty)
;;
