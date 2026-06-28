include Stdlib.Result
include Stdlib.Result.Syntax

let ( >>= ) = bind

let sequence (xs : ('a, 'e) result list) : ('a list, 'e) result =
  List.fold_right
    (fun ma mfa ->
       let* a = ma in
       let* fa = mfa in
       Ok (a :: fa))
    xs
    (Ok [])
;;

let map_m (f : 'a -> ('b, 'e) result) (xs : 'a list) : ('b list, 'e) result =
  xs |> List.map f |> sequence
;;
