open Syntax

let rec matches value pattern =
  let open Value in
  let ( let* ) = Option.bind in
  match value, pattern with
  | VInt n, PInt n' when n = n' -> Some Env.empty
  | VBool b, PBool b' when b = b' -> Some Env.empty
  | value, PVar name -> Some (Env.singleton name value)
  | VPair (v1, v2), PPair (p1, p2) ->
    let* e1 = matches v1 p1 in
    let* e2 = matches v2 p2 in
    Some (Env.union e1 e2)
  | VList [], PNil -> Some Env.empty
  | VList (vhd :: vtl), PCons (phd, ptl) ->
    let* ehd = matches vhd phd in
    let* etl = matches (VList vtl) ptl in
    Some (Env.union ehd etl)
  | _ -> None
;;

let extend_with_r decls env =
  let open Result in
  let* nenv =
    decls
    |> List.mapi (fun i (self, expr) ->
      match expr with
      | EFun _ -> Ok (self, Value.VRecFun (i, decls, env))
      | _ -> Error Error.LetRecForNonFunc)
    |> Result.sequence
  in
  Ok (nenv |> Env.of_list |> Env.union env)
;;

type 'a cont = ('a -> (Value.t, Error.t) result) -> (Value.t, Error.t) result

let rec eval_expr env expr k =
  let open Result in
  let open Value in
  match expr with
  | EConstInt n -> Ok (VInt n) >>= k
  | EConstBool b -> Ok (VBool b) >>= k
  | EVar name -> Env.lookup name env >>= k
  | EAdd (l, r) ->
    eval_expr env l (fun l ->
      let* l = expect_int l in
      eval_expr env r (fun r ->
        let* r = expect_int r in
        k @@ VInt (l + r)))
  | ESub (l, r) ->
    eval_expr env l (fun l ->
      let* l = expect_int l in
      eval_expr env r (fun r ->
        let* r = expect_int r in
        k @@ VInt (l - r)))
  | EMul (l, r) ->
    eval_expr env l (fun l ->
      let* l = expect_int l in
      eval_expr env r (fun r ->
        let* r = expect_int r in
        k @@ VInt (l * r)))
  | EDiv (l, r) ->
    eval_expr env l (fun l' ->
      let* l' = expect_int l' in
      eval_expr env r (fun r' ->
        let* r' = expect_int r' in
        if r' = 0
        then Error (DivisionByZero (string_of_expr (EDiv (l, r))))
        else k @@ VInt (l' / r')))
  | EEq (l, r) -> eval_expr env l (fun l -> eval_expr env r (fun r -> k @@ VBool (l = r)))
  | ELt (l, r) ->
    eval_expr env l (fun l ->
      let* l = expect_int l in
      eval_expr env r (fun r ->
        let* r = expect_int r in
        k @@ VBool (l < r)))
  | EAnd (l, r) ->
    eval_expr env l (fun l ->
      let* l = expect_bool l in
      if l
      then
        eval_expr env r (fun r ->
          let* r = expect_bool r in
          k @@ VBool r)
      else k @@ VBool true)
  | EOr (l, r) ->
    eval_expr env l (fun l ->
      let* l = expect_bool l in
      if l
      then k @@ VBool true
      else
        eval_expr env r (fun r ->
          let* r = expect_bool r in
          k @@ VBool r))
  | EIf (cnd, thn, els) ->
    eval_expr env cnd (fun cnd ->
      let* cnd = expect_bool cnd in
      if cnd then eval_expr env thn k else eval_expr env els k)
  | ELet (decls, body) ->
    (* TODO: multiple declarations *)
    assert (List.length decls = 1);
    let name, expr = List.hd decls in
    eval_expr env expr (fun value ->
      let env' = Env.extend name value env in
      eval_expr env' body k)
  | ELetRec (decls, body) ->
    let* env' = extend_with_r decls env in
    eval_expr env' body k
  | ENil -> k @@ VList []
  | ECons (hd, tl) ->
    eval_expr env hd (fun hd ->
      eval_expr env tl (fun tl ->
        let* tl = expect_list tl in
        k @@ VList (hd :: tl)))
  | EPair (l, r) ->
    eval_expr env l (fun l -> eval_expr env r (fun r -> k @@ VPair (l, r)))
  | EMatch (target, branches) ->
    eval_expr env target (fun value ->
      let matched =
        List.find_map
          (fun (pattern, expr) ->
             matches value pattern |> Option.map (fun binding -> expr, binding))
          branches
      in
      let* expr, binding =
        Option.to_result ~none:(Error.MatchFailure (string_of_expr target)) matched
      in
      eval_expr (Env.union env binding) expr k)
  | EFun (arg, expr) -> k @@ VFun (arg, expr, env)
  | EApp (func, param) ->
    eval_expr env func (fun vfunc ->
      eval_expr env param (fun vparam ->
        match vfunc with
        | VFun (arg, expr, oenv) ->
          let env' = oenv |> Env.extend arg vparam in
          eval_expr env' expr k
        | VRecFun (idxf, mutrefs, oenv) ->
          let env' =
            mutrefs
            |> List.mapi (fun i (self, expr) -> self, VRecFun (i, mutrefs, oenv))
            |> Env.of_list
            |> Env.union oenv
          in
          let _, recfun = List.nth mutrefs idxf in
          eval_expr env' recfun (fun recfun ->
            let* arg, expr, _ = expect_fun recfun in
            let env'' = env' |> Env.extend arg vparam in
            eval_expr env'' expr k)
        | _ -> Error (UnexpectedType (tag_of_value vfunc, "fun"))))
;;

let eval_command env command =
  let open Result in
  match command with
  | CExp expr ->
    let* value = eval_expr env expr ok in
    Ok ([ "-", Value.string_of_value value ], env)
  | CDecl decls ->
    let* evaled =
      decls
      |> Result.map_m (fun (name, expr) ->
        let* value = eval_expr env expr ok in
        Ok (name, value))
    in
    let* output =
      evaled
      |> Result.map_m (fun (name, value) ->
        Ok ("val " ^ string_of_name name, Value.string_of_value value))
    in
    let env' = evaled |> Env.of_list |> Env.union env in
    Ok (output, env')
  | CDeclRec decls ->
    let* env' = extend_with_r decls env in
    Ok (decls |> List.map (fun (name, _) -> "val " ^ string_of_name name, "<fun>"), env')
;;
