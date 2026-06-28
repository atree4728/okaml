type t = (Type.t * Type.t) list

let rec unify =
  let open Result in
  let open Type in
  function
  | [] -> Ok []
  | (s, t) :: rest when s = t -> unify rest
  | (TyFun (s, t), TyFun (s', t')) :: rest -> unify @@ ((s, s') :: (t, t') :: rest)
  | (TyPair (s, t), TyPair (s', t')) :: rest -> unify @@ ((s, s') :: (t, t') :: rest)
  | (TyList s, TyList t) :: rest -> unify @@ ((s, t) :: rest)
  | (TyVar a, t) :: rest | (t, TyVar a) :: rest ->
    if Tyvar.appear_in a t
    then Error (Error.RecursiveType (string_of_tyvar a, string_of_type t))
    else (
      let sub = Subst.apply [ a, t ] in
      let* subed = unify (List.map (fun (t1, t2) -> sub t1, sub t2) rest) in
      Ok (Subst.compose subed [ a, t ]))
  | (s, t) :: rest -> Error (UnexpectedType (string_of_type s, string_of_type t))
;;
