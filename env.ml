module M = Map.Make (String)

let empty = Types.Env M.empty

let extend (Types.Name name) v (Types.Env env) =
  Types.Env (if String.starts_with ~prefix:"_" name then env else M.add name v env)
;;

let singleton name v = extend name v empty

let of_list bindings =
  List.fold_left (fun m (name, expr) -> extend name expr m) empty bindings
;;

let to_list (Types.Env env) = M.to_list env

(* NOTE: the latter precedes if conflicts exist *)
let union (Types.Env e1) (Types.Env e2) =
  Types.Env (M.union (fun _ _ v2 -> Some v2) e1 e2)
;;

let lookup (Types.Name name) (Types.Env env) =
  M.find_opt name env |> Option.to_result ~none:(Types.Unbound name)
;;

let map f (Types.Env env) = Types.Env (M.map f env)
