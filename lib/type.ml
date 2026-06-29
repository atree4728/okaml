type tyvar = Idx of int

type t =
  | TyInt
  | TyBool
  | TyFun of t * t * t * t (* τ₁ / α → τ₂ / β *)
  | TyVar of tyvar
  | TyPair of t * t
  | TyList of t

type schema = Schema of tyvar list * t

let schema_of ty = Schema ([], ty)
let tyvar_of n = Idx n
let string_of_tyvar (Idx i) = Printf.sprintf "'a%d" i

let string_of_type' pretty ty =
  let seen = ref [] in
  let name_of tyvar =
    match List.assoc_opt tyvar !seen with
    | Some s -> s
    | None ->
      let s =
        let n = List.length !seen in
        if n < 26
        then Printf.sprintf "'%c" (Char.chr (Char.code 'a' + n))
        else Printf.sprintf "'a%d" (n - 26)
      in
      seen := (tyvar, s) :: !seen;
      s
  in
  (* 
     priority:
     (1) _ -> _ [left-assoc]
     (2) _ / _
     (3) _ list
     (4) _ * _
     (5) int, bool, tyvar
  *)
  let rec aux ctx ty =
    let priority, s =
      match ty with
      | TyInt -> 5, "int"
      | TyBool -> 5, "bool"
      | TyVar v -> 5, if pretty then name_of v else string_of_tyvar v
      | TyList t -> 4, Printf.sprintf "%s list" (aux 4 t)
      | TyPair (t1, t2) ->
        let s1 = aux 4 t1 in
        let s2 = aux 4 t2 in
        3, Printf.sprintf "%s * %s" s1 s2
      | TyFun (t1, TyVar (Idx a), t2, TyVar (Idx b)) when a = b ->
        let s1 = aux 2 t1 in
        let s2 = aux 1 t2 in
        1, Printf.sprintf "%s -> %s" s1 s2
      | TyFun (t1, a, t2, b) ->
        let s1 = aux 3 t1 in
        let a = aux 3 a in
        let s2 = aux 3 t2 in
        let b = aux 3 b in
        1, Printf.sprintf "%s / %s -> %s / %s" s1 a s2 b
    in
    if priority < ctx then Printf.sprintf "(%s)" s else s
  in
  aux 0 ty
;;

let pretty_of_type = string_of_type' true
let string_of_type = string_of_type' false

let string_of_type_schema (Schema (abs_tyvars, ty)) =
  Printf.sprintf
    "∀ %s. %s"
    (abs_tyvars |> List.map string_of_tyvar |> String.concat ", ")
    (string_of_type ty)
;;
