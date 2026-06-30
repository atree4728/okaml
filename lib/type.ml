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
let answer_all = true (* corresponds to #answer "all" / "none" in OchaCaml *)

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
     Maintain the current operator precedence
     and print parentheses around an operator
     only if its precedence is less than the current precedence.

     c.f. https://ocaml.org/manual/5.4/coreexamples.html#s%3Apretty-printing

     priority:
     (0) _ -> _ [left-assoc]
     (1) _ / _
     (2) _ list
     (3) _ * _
     (4) int, bool, tyvar
  *)
  let is_purity_witness t t' =
    let rec aux = function
      | TyInt | TyBool -> 0
      | TyFun (t1, a, t2, b) -> aux t1 + aux a + aux t2 + aux b
      | TyVar name -> if name = t then 1 else 0
      | TyPair (t1, t2) -> aux t1 + aux t2
      | TyList t -> aux t
    in
    pretty && t = t' && aux ty = 2
  in
  let wrap paren s = if paren then Printf.sprintf "(%s)" s else s in
  let rec aux ctx = function
    | TyInt -> "int"
    | TyBool -> "bool"
    | TyVar v -> if pretty then name_of v else string_of_tyvar v
    | TyList t -> Printf.sprintf "%s list" (aux 3 t) |> wrap (ctx > 3)
    | TyPair (t1, t2) ->
      let s1 = aux 3 t1 in
      let s2 = aux 3 t2 in
      Printf.sprintf "%s * %s" s1 s2 |> wrap (ctx > 2)
    | TyFun (t1, TyVar a, t2, TyVar b) when is_purity_witness a b ->
      let s1 = aux 1 t1 in
      let s2 = aux 0 t2 in
      Printf.sprintf "%s -> %s" s1 s2 |> wrap (ctx > 0)
    | TyFun (t1, a, t2, b) ->
      if answer_all
      then (
        let s1 = aux 2 t1 in
        let a = aux 2 a in
        let s2 = aux 2 t2 in
        let b = aux 2 b in
        Printf.sprintf "%s / %s -> %s / %s" s1 a s2 b |> wrap (ctx > 0))
      else (
        let s1 = aux 1 t1 in
        let s2 = aux 0 t2 in
        Printf.sprintf "%s => %s" s1 s2 |> wrap (ctx > 0))
  in
  aux 0 ty
;;

let string_of_type = string_of_type' false
let pretty_of_type = string_of_type' true
