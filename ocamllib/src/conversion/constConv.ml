open Const

let to_string (c : t) : string =
  match c with
  | Bool b -> string_of_bool b
  | Int i -> string_of_int i
  | Nil -> "[]"
