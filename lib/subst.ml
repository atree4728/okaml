type t = (Type.tyvar * Type.t) list

let apply_to_schema sub (Type.Schema (abs_tyvars, ty)) =
  let open Type in
  let rec aux = function
    | TyInt -> TyInt
    | TyBool -> TyBool
    | TyFun (t1, t2) -> TyFun (aux t1, aux t2)
    | TyVar tyvar ->
      let sub' = List.filter (fun (name, _) -> not @@ List.mem name abs_tyvars) sub in
      List.assoc_opt tyvar sub' |> Option.value ~default:(TyVar tyvar)
    | TyPair (t1, t2) -> TyPair (aux t1, aux t2)
    | TyList t -> TyList (aux t)
    | TyToplevel -> TyToplevel
  in
  Type.Schema (abs_tyvars, aux ty)
;;

let apply sub ty =
  let (Type.Schema (_, ty')) = apply_to_schema sub (Type.schema_of ty) in
  ty'
;;

let compose sub1 sub2 =
  let from_sub1 =
    sub1 |> List.filter (fun (tyvar, _) -> List.assoc_opt tyvar sub2 = None)
  in
  let from_sub2 = List.map (fun (tyvar, ty) -> tyvar, apply sub1 ty) sub2 in
  from_sub1 @ from_sub2
;;
