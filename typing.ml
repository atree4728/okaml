open Syntax

let generalize tyenv ty = Type.Schema (Tyvar.frees tyenv ty, ty)

let instantiate (Type.Schema (abs_tyvars, ty)) =
  let concretize =
    abs_tyvars
    |> List.map (fun abs_tyvar -> abs_tyvar, Type.TyVar (Tyvar.fresh ()))
    |> Subst.apply
  in
  concretize ty
;;

let rec infer_patten =
  let open Result in
  let open Type in
  function
  | PInt _ -> Ok (TyInt, [], Env.empty)
  | PBool _ -> Ok (TyBool, [], Env.empty)
  | PVar name ->
    let tv = Tyvar.fresh () in
    let type_schema = schema_of @@ TyVar tv in
    let new_binding = Env.singleton name type_schema in
    Ok (TyVar tv, [], new_binding)
  | PPair (l, r) ->
    let* type_l, constr_l, new_binding_l = infer_patten l in
    let* type_r, constr_r, new_binding_r = infer_patten r in
    let constr = constr_l @ constr_r in
    let new_binding = Env.union new_binding_l new_binding_r in
    Ok (TyPair (type_l, type_r), constr, new_binding)
  | PNil ->
    let type_list = TyList (TyVar (Tyvar.fresh ())) in
    Ok (type_list, [], Env.empty)
  | PCons (hd, tl) ->
    let* type_hd, constr_hd, new_binding_hd = infer_patten hd in
    let* type_tl, constr_tl, new_binding_tl = infer_patten tl in
    let constr = ((TyList type_hd, type_tl) :: constr_hd) @ constr_tl in
    Ok (type_tl, constr, Env.union new_binding_hd new_binding_tl)
;;

let rec infer_branch tyenv (pattern, expr) =
  let open Result in
  let* type_pattern, constr_pattern, new_binding = infer_patten pattern in
  let tyenv' = Env.union tyenv new_binding in
  let* type_expr, constr_expr = infer_expr tyenv' expr in
  Ok (type_pattern, type_expr, constr_pattern @ constr_expr)

and infer_let is_rec tyenv decls cont =
  let open Result in
  (* generate type variables and extend the environment with them, for the case of let rec *)
  let tyenv' =
    if is_rec
    then
      decls
      |> List.map (fun (name, _) -> name, Type.schema_of @@ Type.TyVar (Tyvar.fresh ()))
      |> Env.of_list
      |> Env.union tyenv
    else tyenv
  in
  let* inferred_triple =
    decls
    |> map_m (fun (name, expr) ->
      let* ty, constr = infer_expr tyenv' expr in
      if is_rec
      then
        (* link ty_placeholder to recursive uses *)
        let* (Type.Schema (_, ty_placeholder)) = Env.lookup name tyenv' in
        Ok (name, ty, (ty, ty_placeholder) :: constr)
      else Ok (name, ty, constr))
  in
  let constr_bindings = List.concat_map (fun (_, _, constr) -> constr) inferred_triple in
  let* subst = Constraints.unify constr_bindings in
  (* weak type variables *)
  (* the target of unifing is tyenv, not tyenv', because tyenv' necessarily knows binding targets so they cannot be polymorphic. *)
  let unified_tyenv = Env.map (Subst.apply_to_schema subst) tyenv in
  let new_tyenv =
    inferred_triple
    |> List.map (fun (name, ty, _) ->
      let unified_ty = Subst.apply subst ty in
      let abs_tyvars = Tyvar.frees unified_tyenv unified_ty in
      name, Type.Schema (abs_tyvars, unified_ty))
    |> Env.of_list
    |> Env.union unified_tyenv
  in
  let* type_cont, constr_cont = infer_expr new_tyenv cont in
  Ok (type_cont, constr_cont @ constr_bindings)

and infer_expr tyenv =
  let open Result in
  let open Type in
  function
  | EConstInt _ -> Ok (TyInt, [])
  | EConstBool _ -> Ok (TyBool, [])
  | EVar name ->
    let* ty = Env.lookup name tyenv in
    Ok (instantiate ty, [])
  | EAdd (l, r) | ESub (l, r) | EMul (l, r) | EDiv (l, r) ->
    let* type_l, constr_l = infer_expr tyenv l in
    let* type_r, constr_r = infer_expr tyenv r in
    Ok (TyInt, ((type_l, TyInt) :: (type_r, TyInt) :: constr_l) @ constr_r)
  | ELt (l, r) ->
    let* type_l, constr_l = infer_expr tyenv l in
    let* type_r, constr_r = infer_expr tyenv r in
    Ok (TyBool, ((type_l, TyInt) :: (type_r, TyInt) :: constr_l) @ constr_r)
  | EEq (l, r) ->
    let* type_l, constr_l = infer_expr tyenv l in
    let* type_r, constr_r = infer_expr tyenv r in
    Ok (TyBool, ((type_l, type_r) :: constr_l) @ constr_r)
  | EAnd (l, r) | EOr (l, r) ->
    let* type_l, constr_l = infer_expr tyenv l in
    let* type_r, constr_r = infer_expr tyenv r in
    Ok (TyBool, ((type_l, TyBool) :: (type_r, TyBool) :: constr_l) @ constr_r)
  | EIf (cnd, thn, els) ->
    let* type_cnd, constr_cnd = infer_expr tyenv cnd in
    let* type_thn, constr_thn = infer_expr tyenv thn in
    let* type_els, constr_els = infer_expr tyenv els in
    let constr =
      ((type_cnd, TyBool) :: (type_thn, type_els) :: constr_cnd) @ constr_thn @ constr_els
    in
    Ok (type_thn, constr)
  | ELet (decls, cont) -> infer_let false tyenv decls cont
  | ELetRec (decls, cont) -> infer_let true tyenv decls cont
  | ENil -> Ok (TyList (TyVar (Tyvar.fresh ())), [])
  | ECons (hd, tl) ->
    let* type_hd, constr_hd = infer_expr tyenv hd in
    let* type_tl, constr_tl = infer_expr tyenv tl in
    Ok (type_tl, ((TyList type_hd, type_tl) :: constr_hd) @ constr_tl)
  | EPair (l, r) ->
    let* type_l, constr_l = infer_expr tyenv l in
    let* type_r, constr_r = infer_expr tyenv r in
    Ok (TyPair (type_l, type_r), constr_l @ constr_r)
  | EMatch (target, branches) ->
    let* type_target, constr_target = infer_expr tyenv target in
    let type_ret = TyVar (Tyvar.fresh ()) in
    let* constr =
      branches
      |> Result.map_m (fun branch ->
        let* type_pattern, type_expr, constr_branch = infer_branch tyenv branch in
        Ok ((type_target, type_pattern) :: (type_ret, type_expr) :: constr_branch))
      |> Result.map List.concat
    in
    Ok (type_ret, constr)
  | EFun (arg, expr) ->
    let type_arg = TyVar (Tyvar.fresh ()) in
    (* do not generalize *)
    let tyenv' = Env.extend arg (schema_of type_arg) tyenv in
    let* type_expr, constr_expr = infer_expr tyenv' expr in
    Ok (TyFun (type_arg, type_expr), constr_expr)
  | EApp (func, param) ->
    let* type_func, constr_func = infer_expr tyenv func in
    let* type_param, constr_param = infer_expr tyenv param in
    let type_ret = TyVar (Tyvar.fresh ()) in
    let constr =
      ((type_func, TyFun (type_param, type_ret)) :: constr_func) @ constr_param
    in
    Ok (type_ret, constr)
;;

let infer_letcmd is_rec tyenv decls =
  let open Result in
  let* ty_bindings =
    decls
    |> Result.map_m (fun (name, expr) ->
      let dummy_expr =
        if is_rec then ELetRec (decls, EVar name) else ELet ([ name, expr ], EVar name)
      in
      let* type_cont, constraints = infer_expr tyenv dummy_expr in
      let* subst = Constraints.unify constraints in
      Ok (name, Subst.apply subst type_cont))
  in
  let tys = List.map snd ty_bindings in
  let new_tyenv =
    ty_bindings
    |> List.map (fun (name, ty) -> name, generalize tyenv ty)
    |> Env.of_list
    |> Env.union tyenv
  in
  Ok (tys, new_tyenv)
;;

let infer_cmd tyenv command =
  let open Result in
  match command with
  | CExp expr ->
    let* type_expr, constraints = infer_expr tyenv expr in
    let* subst = Constraints.unify constraints in
    let unified = Subst.apply subst type_expr in
    Ok ([ unified ], tyenv)
  | CDecl decls -> infer_letcmd false tyenv decls
  | CDeclRec decls -> infer_letcmd true tyenv decls
;;
