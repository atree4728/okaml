module M = Map.Make (String)

type 'a t = Env of 'a M.t

let empty = Env M.empty

let extend (Syntax.Name name) v (Env env) =
  Env (if name = "_" then env else M.add name v env)
;;

let singleton name v = extend name v empty

let of_list bindings =
  List.fold_left (fun m (name, expr) -> extend name expr m) empty bindings
;;

let to_list (Env env) = M.to_list env

(* NOTE: the latter precedes if conflicts exist *)
let union (Env e1) (Env e2) = Env (M.union (fun _ _ v2 -> Some v2) e1 e2)

let exclusive_union (Env e1) (Env e2) =
  let exception Duplicate of string in
  match M.union (fun name _ v2 -> raise (Duplicate name)) e1 e2 with
  | merged -> Ok (Env merged)
  | exception Duplicate name -> Error (Error.DuplicatedBound name)
;;

let lookup (Syntax.Name name) (Env env) =
  M.find_opt name env |> Option.to_result ~none:(Error.Unbound name)
;;

let map f (Env env) = Env (M.map f env)
