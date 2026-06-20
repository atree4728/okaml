open Types

let rec matches value pattern =
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

let expect_int = function
  | VInt n -> Ok n
  | v -> Error (UnexpectedType (tag_of_value v, "int"))
;;

let expect_bool = function
  | VBool b -> Ok b
  | v -> Error (UnexpectedType (tag_of_value v, "bool"))
;;

let expect_list = function
  | VList l -> Ok l
  | v -> Error (UnexpectedType (tag_of_value v, "list"))
;;

let expect_fun = function
  | VFun (args, expr, env) -> Ok (args, expr, env)
  | v -> Error (UnexpectedType (tag_of_value v, "fun"))
;;

let expect_recfun = function
  | VRecFun (i, mutrefs, env) -> Ok (i, mutrefs, env)
  | v -> Error (UnexpectedType (tag_of_value v, "recfun"))
;;

let extend_with_r decls env =
  let open Myresult in
  let* nenv =
    decls
    |> List.mapi (fun i (self, expr) ->
      match expr with
      | EFun _ -> Ok (self, VRecFun (i, decls, env))
      | _ -> Error LetRecForNonFunc)
    |> sequence
  in
  Ok (nenv |> Env.of_list |> Env.union env)
;;

let rec eval_expr env e =
  let open Myresult in
  match e with
  | EConstInt n -> Ok (VInt n)
  | EConstBool b -> Ok (VBool b)
  | EVar name -> Env.lookup name env
  | EAdd (l, r) ->
    let* l = eval_expr env l >>= expect_int in
    let* r = eval_expr env r >>= expect_int in
    Ok (VInt (l + r))
  | ESub (l, r) ->
    let* l = eval_expr env l >>= expect_int in
    let* r = eval_expr env r >>= expect_int in
    Ok (VInt (l - r))
  | EMul (l, r) ->
    let* l = eval_expr env l >>= expect_int in
    let* r = eval_expr env r >>= expect_int in
    Ok (VInt (l * r))
  | EDiv (l, r) ->
    let* l' = eval_expr env l >>= expect_int in
    let* r' = eval_expr env r >>= expect_int in
    if r' = 0
    then Error (DivisionByZero (string_of_expr (EDiv (l, r))))
    else Ok (VInt (l' / r'))
  | EEq (l, r) ->
    let* l = eval_expr env l in
    let* r = eval_expr env r in
    Ok (VBool (l = r))
  | ELt (l, r) ->
    let* l = eval_expr env l >>= expect_int in
    let* r = eval_expr env r >>= expect_int in
    Ok (VBool (l < r))
  | EAnd (l, r) ->
    let* l = eval_expr env l >>= expect_bool in
    if l
    then eval_expr env r >>= expect_bool >>= fun b -> Ok (VBool b)
    else Ok (VBool false)
  | EOr (l, r) ->
    let* l = eval_expr env l >>= expect_bool in
    if l
    then Ok (VBool true)
    else eval_expr env r >>= expect_bool >>= fun b -> Ok (VBool b)
  | EIf (cnd, thn, els) ->
    let* bcnd = eval_expr env cnd >>= expect_bool in
    if bcnd then eval_expr env thn else eval_expr env els
  | ELet (decls, cont) ->
    let* env' = Result.map fst @@ extend_with decls env in
    eval_expr env' cont
  | ELetRec (decls, cont) ->
    let* env' = extend_with_r decls env in
    eval_expr env' cont
  | ENil -> Ok (VList [])
  | ECons (hd, tl) ->
    let* vhd = eval_expr env hd in
    let* vtl = eval_expr env tl >>= expect_list in
    Ok (VList (vhd :: vtl))
  | EPair (l, r) ->
    let* l = eval_expr env l in
    let* r = eval_expr env r in
    Ok (VPair (l, r))
  | EMatch (target, branches) ->
    let* value = eval_expr env target in
    let matched =
      List.find_map
        (fun (pattern, expr) ->
           matches value pattern |> Option.map (fun binding -> expr, binding))
        branches
    in
    let* expr, binding =
      Option.to_result ~none:(MatchFailure (string_of_expr target)) matched
    in
    eval_expr (Env.union env binding) expr
  | EFun (arg, expr) -> Ok (VFun (arg, expr, env))
  | EApp (func, param) ->
    let* vfunc = eval_expr env func in
    let* vparam = eval_expr env param in
    alternative
      (let* arg, expr, oenv = expect_fun vfunc in
       let env' = oenv |> Env.extend arg vparam in
       eval_expr env' expr)
      (let* idxf, mutrefs, oenv = expect_recfun vfunc in
       let env' =
         mutrefs
         |> List.mapi (fun i (self, expr) -> self, VRecFun (i, mutrefs, oenv))
         |> Env.of_list
         |> Env.union oenv
       in
       let _, recfun = List.nth mutrefs idxf in
       let* arg, expr, _ = eval_expr env' recfun >>= expect_fun in
       let env'' = env' |> Env.extend arg vparam in
       eval_expr env'' expr)

and extend_with decls env =
  let open Myresult in
  let* evaled =
    map_m
      (fun (name, expr) ->
         let* value = eval_expr env expr in
         Ok (name, value))
      decls
  in
  Ok (Env.union env (Env.of_list evaled), evaled)
;;

let eval_command env command =
  let open Myresult in
  match command with
  | CExp expr ->
    let* value = eval_expr env expr in
    Ok ([ "-", string_of_value value ], env)
  | CDecl decls ->
    let* env', bindings = extend_with decls env in
    Ok
      ( bindings
        |> List.map (fun (name, value) ->
          "val " ^ string_of_name name, string_of_value value)
      , env' )
  | CDeclRec decls ->
    let* env' = extend_with_r decls env in
    Ok (decls |> List.map (fun (name, _) -> "val " ^ string_of_name name, "<fun>"), env')
;;
