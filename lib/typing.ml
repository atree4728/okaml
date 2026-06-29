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

let expect_tyfun ty =
  match ty with
  | Type.TyFun (t1, a, t2, b) -> Ok (t1, a, t2, b)
  | _ -> Error (Error.UnexpectedType (Type.string_of_type ty, "fun"))
;;

let rec infer_pattern =
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
    let* type_l, constr_l, new_binding_l = infer_pattern l in
    let* type_r, constr_r, new_binding_r = infer_pattern r in
    let constr = constr_l @ constr_r in
    let* new_binding = Env.exclusive_union new_binding_l new_binding_r in
    Ok (TyPair (type_l, type_r), constr, new_binding)
  | PNil ->
    let type_list = TyList (TyVar (Tyvar.fresh ())) in
    Ok (type_list, [], Env.empty)
  | PCons (hd, tl) ->
    let* type_hd, constr_hd, new_binding_hd = infer_pattern hd in
    let* type_tl, constr_tl, new_binding_tl = infer_pattern tl in
    let constr = ((TyList type_hd, type_tl) :: constr_hd) @ constr_tl in
    let* new_binding = Env.exclusive_union new_binding_hd new_binding_tl in
    Ok (type_tl, constr, new_binding)
;;

let rec infer_branch tyenv answer_type (pattern, expr) =
  let open Result in
  let* type_pattern, constr_pattern, new_binding = infer_pattern pattern in
  let tyenv' = Env.union tyenv new_binding in
  let* type_expr, answer_type', constr_expr = infer_expr tyenv' answer_type expr in
  Ok (type_pattern, answer_type', type_expr, constr_pattern @ constr_expr)

and infer_expr tyenv alpha =
  let open Result in
  let open Type in
  function
  (*
      (c is a constant of basic type b)
      --------------------------------- (const)
              Γ; α ⊢ c : b; α
    *)
  | EConstInt _ -> Ok (TyInt, alpha, [])
  | EConstBool _ ->
    Ok (TyBool, alpha, [])
    (*
      x : A ∈ Γ and τ is an instance of A
      ----------------------------------- (var)
              Γ; α ⊢ x : τ; α
    *)
  | EVar name ->
    let* ty = Env.lookup name tyenv in
    Ok (instantiate ty, alpha, [])
    (* TODO *)
  | EAdd (l, r) | ESub (l, r) | EMul (l, r) | EDiv (l, r) ->
    let* type_l, bns, constr_l = infer_expr tyenv alpha l in
    let* type_r, cns, constr_r = infer_expr tyenv bns r in
    Ok (TyInt, cns, ((type_l, TyInt) :: (type_r, TyInt) :: constr_l) @ constr_r)
  | ELt (l, r) ->
    let* type_l, bns, constr_l = infer_expr tyenv alpha l in
    let* type_r, cns, constr_r = infer_expr tyenv bns r in
    Ok (TyBool, cns, ((type_l, TyInt) :: (type_r, TyInt) :: constr_l) @ constr_r)
  | EEq (l, r) ->
    let* type_l, bns, constr_l = infer_expr tyenv alpha l in
    let* type_r, cns, constr_r = infer_expr tyenv bns r in
    Ok (TyBool, cns, ((type_l, type_r) :: constr_l) @ constr_r)
  | EAnd (l, r) | EOr (l, r) ->
    let* type_l, answer_type, constr_l = infer_expr tyenv alpha l in
    let* type_r, answer_type, constr_r = infer_expr tyenv answer_type r in
    Ok (TyBool, answer_type, ((type_l, TyBool) :: (type_r, TyBool) :: constr_l) @ constr_r)
    (*
      Γ ; σ ⊢ cnd : bool ; β    Γ ; α ⊢ thn : τ ; σ    Γ ; α ⊢ els : τ ; σ
      ----------------------------------------------------------------- (if)
                     Γ ; α ⊢ if cnd then thn else els : τ ; β
    *)
  | EIf (cnd, thn, els) ->
    let* type_cnd, answer_type, constr_cnd = infer_expr tyenv alpha cnd in
    let* type_thn, answer_type_thn, constr_thn = infer_expr tyenv answer_type thn in
    let* type_els, answer_type_els, constr_els = infer_expr tyenv answer_type els in
    let constr =
      ((type_cnd, TyBool)
       :: (type_thn, type_els)
       :: (answer_type_thn, answer_type_els)
       :: constr_cnd)
      @ constr_thn
      @ constr_els
    in
    Ok (type_thn, answer_type_thn, constr)
  | ELet (decls, body) -> infer_let false tyenv alpha decls body
  | ELetRec (decls, body) -> infer_let true tyenv alpha decls body
  | ENil -> Ok (TyList (TyVar (Tyvar.fresh ())), alpha, [])
  | ECons (hd, tl) ->
    let* type_hd, answer_type, constr_hd = infer_expr tyenv alpha hd in
    let* type_tl, answer_type, constr_tl = infer_expr tyenv answer_type tl in
    Ok (type_tl, answer_type, ((TyList type_hd, type_tl) :: constr_hd) @ constr_tl)
  | EPair (l, r) ->
    let* type_l, answer_type, constr_l = infer_expr tyenv alpha l in
    let* type_r, answer_type, constr_r = infer_expr tyenv answer_type r in
    Ok (TyPair (type_l, type_r), answer_type, constr_l @ constr_r)
  | EMatch (target, branches) ->
    let* type_target, answer_type, constr_target = infer_expr tyenv alpha target in
    let type_ret = TyVar (Tyvar.fresh ()) in
    let answer_type_whole = TyVar (Tyvar.fresh ()) in
    let* constr =
      branches
      |> Result.map_m (fun branch ->
        let* type_pattern, answer_type, type_expr, constr_branch =
          infer_branch tyenv answer_type branch
        in
        Ok
          ((type_target, type_pattern)
           :: (type_ret, type_expr)
           :: (answer_type, answer_type_whole)
           :: constr_branch))
      |> Result.map List.concat
    in
    Ok (type_ret, answer_type_whole, constr)
    (*
         Γ, arg : σ; β ⊢ expr : τ; γ
      --------------------------------- (fun)
      Γ, α ⊢ fun arg -> expr : (σ/β → τ/γ), α
    *)
  | EFun (arg, expr) ->
    let beta = TyVar (Tyvar.fresh ()) in
    let sigma = TyVar (Tyvar.fresh ()) in
    (* do not generalize *)
    let tyenv' = Env.extend arg (schema_of sigma) tyenv in
    let* tau, gamma, constr = infer_expr tyenv' beta expr in
    Ok (TyFun (sigma, beta, tau, gamma), alpha, constr)
    (*
      Γ ; γ ⊢ func : (σ/α → τ/β) ; δ    Γ ; β ⊢ param : σ ; γ
      ------------------------------------------------------- (app)
                        Γ ; α ⊢ func param : τ ; δ
    *)
  | EApp (func, param) ->
    let beta = TyVar (Tyvar.fresh ()) in
    let* sigma, gamma, constr_param = infer_expr tyenv beta param in
    let* type_func, delta, constr_func = infer_expr tyenv gamma func in
    let tau = TyVar (Tyvar.fresh ()) in
    let constr =
      ((type_func, TyFun (sigma, alpha, tau, beta)) :: constr_func) @ constr_param
    in
    Ok (tau, delta, constr)
    (*
                Γ; σ ⊢ e : σ; τ
      --------------------------------- (reset)
      Γ; α ⊢ reset (fun () -> e) : τ; α
    *)
  | EReset expr ->
    let sigma = TyVar (Tyvar.fresh ()) in
    let* sigma', tau, constr = infer_expr tyenv sigma expr in
    Ok (tau, alpha, (sigma, sigma') :: constr)
    (*
      Γ, k : Γ, k : ∀ t. (τ/t → α/t); σ ⊢ e : σ; β
      ------------------------------------------------ (shift)
                Γ; α ⊢ shift (fun k → e) : τ; β
    *)
  | EShift (k, expr) ->
    let tau = TyVar (Tyvar.fresh ()) in
    let tyenv' =
      let t = Tyvar.fresh () in
      Env.extend k (Schema ([ t ], TyFun (tau, TyVar t, alpha, TyVar t))) tyenv
    in
    let sigma = TyVar (Tyvar.fresh ()) in
    let* sigma', beta, constr = infer_expr tyenv' sigma expr in
    Ok (tau, beta, (sigma, sigma') :: constr)

and infer_bindings is_rec tyenv answer_type decls =
  let open Result in
  assert (List.length decls = 1);
  let name, expr = List.hd decls in
  (* generate type variables and extend the environment with them, for the case of let rec *)
  let tyenv' =
    if is_rec
    then Env.extend name (Type.schema_of @@ Type.TyVar (Tyvar.fresh ())) tyenv
    else tyenv
  in
  let purity_witness = Type.TyVar (Tyvar.fresh ()) in
  let* sigma, purity_witness', constr = infer_expr tyenv' purity_witness expr in
  Ok ([ name, sigma ], purity_witness, purity_witness', constr)

(*
    Γ; p ⊢ e1 : σ; p    Γ, x : Gen(σ; Γ); α ⊢ e2 : τ; β
    --------------------------------------------------- (let)
              Γ ; α ⊢ let x = e1 in e2 : τ ; β
*)
and infer_let is_rec tyenv alpha decls body =
  let open Result in
  let* pre_unified, purity_witness, purity_witness', constr_bindings =
    infer_bindings is_rec tyenv alpha decls
  in
  let* subst = Constraints.unify constr_bindings in
  (* purity checking *)
  let* () =
    match Subst.apply subst purity_witness, Subst.apply subst purity_witness' with
    | TyVar p, TyVar p' when p = p' -> Ok ()
    | _ -> Error Error.NonPure
  in
  (* weak type variables *)
  (* the target of unifing is tyenv, not tyenv', because tyenv' necessarily knows binding targets so they cannot be polymorphic. *)
  let unified_tyenv = Env.map (Subst.apply_to_schema subst) tyenv in
  let new_tyenv =
    pre_unified
    |> List.map (fun (name, ty) ->
      let unified_ty = Subst.apply subst ty in
      let abs_tyvars = Tyvar.frees unified_tyenv unified_ty in
      name, Type.Schema (abs_tyvars, unified_ty))
    |> Env.of_list
    |> Env.union unified_tyenv
  in
  let* tau, beta, constr_body = infer_expr new_tyenv alpha body in
  Ok (tau, beta, constr_body @ constr_bindings)
;;

let infer_letcmd is_rec tyenv decls =
  let dummy_answer_type = Type.TyVar (Tyvar.fresh ()) in
  let open Result in
  let* pre_unified, purity_witness, purity_witness', constr_bindings =
    infer_bindings is_rec tyenv dummy_answer_type decls
  in
  let* subst = Constraints.unify constr_bindings in
  (* purity checking *)
  let* () =
    match Subst.apply subst purity_witness, Subst.apply subst purity_witness' with
    | TyVar p, TyVar p' when p = p' -> Ok ()
    | _ -> Error Error.NonPure
  in
  let unified =
    pre_unified |> List.map (fun (name, ty) -> name, ty |> Subst.apply subst)
  in
  let tys = unified |> List.map (fun (_, ty) -> Type.pretty_of_type ty) in
  let new_tyenv =
    unified
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
    let* tys, new_tyenv = infer_letcmd false tyenv [ Name "__DUMMY__", expr ] in
    Ok (tys, new_tyenv)
  | CDecl decls -> infer_letcmd false tyenv decls
  | CDeclRec decls -> infer_letcmd true tyenv decls
;;
