open Types

type subst = (tyvar * ty) list
type constraints = (ty * ty) list

let var_cnt = ref 0

let fresh () =
  var_cnt := !var_cnt + 1;
  Idx !var_cnt
;;

let ty_schema_subst sub (TySchema (abs_tyvars, ty)) =
  let rec aux = function
    | TyInt -> TyInt
    | TyBool -> TyBool
    | TyFun (t1, t2) -> TyFun (aux t1, aux t2)
    | TyVar tyvar ->
      let sub' = List.filter (fun (name, _) -> not @@ List.mem name abs_tyvars) sub in
      List.assoc_opt tyvar sub' |> Option.value ~default:(TyVar tyvar)
    | TyPair (t1, t2) -> TyPair (aux t1, aux t2)
    | TyList t -> TyList (aux t)
  in
  TySchema (abs_tyvars, aux ty)
;;

let ty_subst sub ty =
  let (TySchema (_, ty')) = ty_schema_subst sub (TySchema ([], ty)) in
  ty'
;;

let compose sub1 sub2 =
  let from_sub1 =
    sub1 |> List.filter (fun (tyvar, _) -> List.assoc_opt tyvar sub2 = None)
  in
  let from_sub2 = List.map (fun (tyvar, ty) -> tyvar, ty_subst sub1 ty) sub2 in
  from_sub1 @ from_sub2
;;

let rec tyvars_list ty =
  let rec aux = function
    | TyInt | TyBool -> []
    | TyFun (t1, t2) | TyPair (t1, t2) -> aux t1 @ aux t2
    | TyVar name -> [ name ]
    | TyList t -> aux t
  in
  aux ty |> List.sort_uniq compare
;;

let rec appear_in tyvar ty = List.mem tyvar @@ tyvars_list ty

let free_tyvars tyenv ty =
  let aux prohibited ty =
    ty |> tyvars_list |> List.filter (fun tyvar -> not @@ List.mem tyvar prohibited)
  in
  let bound_tyvars =
    tyenv
    |> Env.to_list
    |> List.concat_map (fun (_, TySchema (abs_tyvars, ty)) -> aux abs_tyvars ty)
  in
  aux bound_tyvars ty
;;

let rec unify =
  let open Myresult in
  function
  | [] -> Ok []
  | (s, t) :: rest when s = t -> unify rest
  | (TyFun (s, t), TyFun (s', t')) :: rest -> unify @@ ((s, s') :: (t, t') :: rest)
  | (TyPair (s, t), TyPair (s', t')) :: rest -> unify @@ ((s, s') :: (t, t') :: rest)
  | (TyList s, TyList t) :: rest -> unify @@ ((s, t) :: rest)
  | (TyVar a, t) :: rest | (t, TyVar a) :: rest ->
    if appear_in a t
    then Error (RecursiveType (string_of_tyvar a, string_of_type t))
    else (
      let sub = ty_subst [ a, t ] in
      let* subed = unify (List.map (fun (t1, t2) -> sub t1, sub t2) rest) in
      Ok (compose subed [ a, t ]))
  | (s, t) :: rest -> Error (UnexpectedType (string_of_type s, string_of_type t))
;;
