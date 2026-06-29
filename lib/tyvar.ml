let var_cnt = ref 0

let fresh () =
  var_cnt := !var_cnt + 1;
  Type.tyvar_of !var_cnt
;;

let rec tyvars_list ty =
  let open Type in
  let rec aux = function
    | TyInt | TyBool -> []
    | TyFun (t1, a, t2, b) -> List.concat_map aux [ t1; a; t2; b ]
    | TyVar name -> [ name ]
    | TyPair (t1, t2) -> List.concat_map aux [ t1; t2 ]
    | TyList t -> aux t
  in
  aux ty |> List.sort_uniq compare
;;

let rec appear_in tyvar ty = List.mem tyvar @@ tyvars_list ty

let frees tyenv ty =
  let aux prohibited ty =
    ty |> tyvars_list |> List.filter (fun tyvar -> not @@ List.mem tyvar prohibited)
  in
  let bound_tyvars =
    tyenv
    |> Env.to_list
    |> List.concat_map (fun (_, Type.Schema (abs_tyvars, ty)) -> aux abs_tyvars ty)
  in
  aux bound_tyvars ty
;;
