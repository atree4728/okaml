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

let polymorphic_let tyenv constr preunified =
  let open Result in
  let* subst = Constraints.unify constr in
  let unified_tyenv = Env.map (Subst.apply_to_schema subst) tyenv in
  let unified = preunified |> List.map (fun (name, ty) -> name, Subst.apply subst ty) in
  let new_tyenv =
    unified
    |> List.map (fun (name, ty) -> name, generalize unified_tyenv ty)
    |> Env.of_list
    |> Env.union unified_tyenv
  in
  Ok (unified, new_tyenv)
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

let rec infer_branch tyenv alpha (pattern, expr) =
  let open Result in
  let* type_pattern, constr_pattern, new_binding = infer_pattern pattern in
  let tyenv' = Env.union tyenv new_binding in
  let* type_expr, beta, constr_expr = infer_expr tyenv' alpha expr in
  Ok (type_pattern, beta, type_expr, constr_pattern @ constr_expr)

and infer_pure tyenv expr =
  let open Result in
  let purity_witness = Type.TyVar (Tyvar.fresh ()) in
  let* preunified, purity_witness', constr = infer_expr tyenv purity_witness expr in
  let* subst = Constraints.unify constr in
  (* purity checking *)
  let* () =
    match Subst.apply subst purity_witness, Subst.apply subst purity_witness' with
    | TyVar p, TyVar p' when p = p' -> Ok ()
    | _ -> Error Error.NonPure
  in
  Ok (preunified, subst, constr)

(*
      Γ; β ⊢ l : int; γ    Γ; α ⊢ r : int; β
      -------------------------------------- (plus)
              Γ; α ⊢ l+r : int; γ
*)
and infer_binop tyenv alpha l r handler =
  let open Result in
  let* ty_r, beta, constr_r = infer_expr tyenv alpha r in
  let* ty_l, gamma, constr_l = infer_expr tyenv beta l in
  let ty, extra = handler ty_l ty_r in
  Ok (ty, gamma, (extra @ constr_l) @ constr_r)

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
  | EAdd (l, r) | ESub (l, r) | EMul (l, r) | EDiv (l, r) ->
    infer_binop tyenv alpha l r (fun l r -> TyInt, [ l, TyInt; r, TyInt ])
  | ELt (l, r) -> infer_binop tyenv alpha l r (fun l r -> TyBool, [ l, TyInt; r, TyInt ])
  | EEq (l, r) -> infer_binop tyenv alpha l r (fun l r -> TyBool, [ l, r ])
  | EAnd (l, r) | EOr (l, r) ->
    infer_binop tyenv alpha l r (fun l r -> TyBool, [ l, TyBool; r, TyBool ])
    (*
      Γ ; σ ⊢ cnd : bool ; β    Γ ; α ⊢ thn : τ ; σ    Γ ; α ⊢ els : τ ; σ
      -------------------------------------------------------------------- (if)
                     Γ ; α ⊢ if cnd then thn else els : τ ; β
    *)
  | EIf (cnd, thn, els) ->
    let* tau, sigma, constr_thn = infer_expr tyenv alpha thn in
    let* tau', sigma', constr_els = infer_expr tyenv alpha els in
    let* bool', beta, constr_cnd = infer_expr tyenv sigma cnd in
    let constr =
      ((bool', TyBool) :: (tau, tau') :: (sigma, sigma') :: constr_cnd)
      @ constr_thn
      @ constr_els
    in
    Ok (tau, beta, constr)
  (*
    Γ; ⊢p e1 : σ   Γ, x : Gen(σ; Γ); α ⊢ e2 : τ; β
    --------------------------------------------------- (let)
              Γ ; α ⊢ let x = e1 in e2 : τ ; β
  *)
  | ELet (decls, body) ->
    let open Result in
    assert (List.length decls = 1);
    let name, expr = List.hd decls in
    let* sigma, subst, constr_bindings = infer_pure tyenv expr in
    let* _, new_tyenv = polymorphic_let tyenv constr_bindings [ name, sigma ] in
    let* tau, beta, constr_body = infer_expr new_tyenv alpha body in
    Ok (tau, beta, constr_body @ constr_bindings)
  | ELetRec (decls, body) ->
    let* preunified, constr_fix = infer_letrec tyenv decls in
    let* _, new_tyenv = polymorphic_let tyenv constr_fix preunified in
    let* tau, beta, constr_body = infer_expr new_tyenv alpha body in
    Ok (tau, beta, constr_body @ constr_fix)
  | ENil -> Ok (TyList (TyVar (Tyvar.fresh ())), alpha, [])
  | ECons (hd, tl) ->
    let* type_tl, beta, constr_tl = infer_expr tyenv alpha tl in
    let* type_hd, gamma, constr_hd = infer_expr tyenv beta hd in
    Ok (type_tl, gamma, ((TyList type_hd, type_tl) :: constr_hd) @ constr_tl)
  | EPair (l, r) ->
    let* type_r, beta, constr_r = infer_expr tyenv alpha r in
    let* type_l, gamma, constr_l = infer_expr tyenv beta l in
    Ok (TyPair (type_l, type_r), gamma, constr_l @ constr_r)
  | EMatch (target, branches) ->
    let sigma = TyVar (Tyvar.fresh ()) in
    let* type_target, beta, constr_target = infer_expr tyenv sigma target in
    let type_ret = TyVar (Tyvar.fresh ()) in
    let* constr =
      branches
      |> Result.map_m (fun branch ->
        let* type_pattern, sigma', type_expr, constr_branch =
          infer_branch tyenv alpha branch
        in
        Ok
          ((type_target, type_pattern)
           :: (type_ret, type_expr)
           :: (sigma, sigma')
           :: constr_branch))
      |> Result.map List.concat
    in
    Ok (type_ret, beta, constr)
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

and infer_letrec tyenv decls =
  let open Result in
  assert (List.length decls = 1);
  let self, fun_expr = List.hd decls in
  let* arg, expr =
    match fun_expr with
    | EFun (arg, expr) -> Ok (arg, expr)
    | _ -> Error Error.LetRecForNonFunc
  in
  (*
    Γ, f : (σ/α → τ/β), x : σ; α ⊢ e : τ; β
    ----------------------------------------- (fix)
          Γ ⊢p fix f.x.e : (σ/α → τ/β)
  *)
  let sigma = Type.TyVar (Tyvar.fresh ()) in
  let alpha = Type.TyVar (Tyvar.fresh ()) in
  let tau = Type.TyVar (Tyvar.fresh ()) in
  let beta = Type.TyVar (Tyvar.fresh ()) in
  let preunified = Type.TyFun (sigma, alpha, tau, beta) in
  let tyenv' =
    tyenv
    |> Env.extend self (Type.schema_of preunified)
    |> Env.extend arg (Type.schema_of sigma)
  in
  let* tau', beta', constr = infer_expr tyenv' alpha expr in
  Ok ([ self, preunified ], (beta, beta') :: (tau, tau') :: constr)
;;

let infer_cmd tyenv command =
  let open Result in
  match command with
  | CExp expr ->
    let* preunified, subst, _ = infer_pure tyenv expr in
    Ok ([ preunified |> Subst.apply subst |> Type.pretty_of_type ], tyenv)
  | CDecl decls ->
    let name, expr = List.hd decls in
    let* preunified, subst, constr = infer_pure tyenv expr in
    let* unified, new_tyenv = polymorphic_let tyenv constr [ name, preunified ] in
    let pretties = unified |> List.map (fun (_, ty) -> Type.pretty_of_type ty) in
    Ok (pretties, new_tyenv)
  | CDeclRec decls ->
    let* preunified, constr_fix = infer_letrec tyenv decls in
    let* unified, new_tyenv = polymorphic_let tyenv constr_fix preunified in
    let pretties = unified |> List.map (fun (_, ty) -> Type.pretty_of_type ty) in
    Ok (pretties, new_tyenv)
;;
