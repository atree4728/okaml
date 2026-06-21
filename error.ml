type t =
  | Unbound of string
  | UnexpectedType of string * string
  | DivisionByZero of string
  | MatchFailure of string
  | DuplicatedBound of string
  | RecursiveType of string * string
  | LetRecForNonFunc

let string_of_error = function
  | Unbound name -> "Error: Unbound value " ^ name
  | UnexpectedType (actual, expected) ->
    Printf.sprintf
      "Error: The value has type `%s` but an expression was expected of type `%s`"
      actual
      expected
  | DivisionByZero expr -> "Error: Division by zero: " ^ expr
  | MatchFailure expr -> "Error: Match failure: " ^ expr
  | DuplicatedBound name ->
    Printf.sprintf "Error: Variable %s is bound several times in this matching" name
  | RecursiveType (tyvar, ty) ->
    Printf.sprintf "Error: Detected a recursive type definition: %s = %s" tyvar ty
  | LetRecForNonFunc -> "Error: `let rec` is allowed only for function binding."
;;
