type t =
  | Unbound of string
  | UnexpectedType of string * string
  | DivisionByZero of string
  | MatchFailure of string
  | DuplicatedBound of string
  | RecursiveType of string * string
  | LetRecForNonFunc
  | NonPure

let string_of_error = function
  | Unbound name -> "Error: Unbound value " ^ name
  | UnexpectedType (actual, expected) ->
    if actual = "toplevel" || expected = "toplevel"
    then "Error: cannot capture continuation in toplevel. Use `reset` to prompt."
    else
      Printf.sprintf
        "Error: The value has type `%s` but an expression was expected of type `%s`."
        actual
        expected
  | DivisionByZero expr -> Printf.sprintf "Error: Division by zero: %s." expr
  | MatchFailure expr -> Printf.sprintf "Error: Match failure: %s." expr
  | DuplicatedBound name ->
    Printf.sprintf "Error: Variable %s is bound several times in this matching." name
  | RecursiveType (tyvar, ty) ->
    Printf.sprintf "Error: Detected a recursive type definition: %s = %s." tyvar ty
  | LetRecForNonFunc -> "Error: `let rec` is allowed only for function binding."
  | NonPure -> "Error: This expression is not pure."
;;
