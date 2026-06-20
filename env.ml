module M = Map.Make (String)

type 'a t = Env of 'a M.t

let empty = Env M.empty

let extend (Syntax.Name name) v (Env env) =
  Env (if String.starts_with ~prefix:"_" name then env else M.add name v env)
;;

let singleton name v = extend name v empty

let of_list bindings =
  List.fold_left (fun m (name, expr) -> extend name expr m) empty bindings
;;

let to_list (Env env) = M.to_list env

(* NOTE: the latter precedes if conflicts exist *)
let union (Env e1) (Env e2) = Env (M.union (fun _ _ v2 -> Some v2) e1 e2)

let lookup (Syntax.Name name) (Env env) =
  M.find_opt name env |> Option.to_result ~none:(Error.Unbound name)
;;

let map f (Env env) = Env (M.map f env)
