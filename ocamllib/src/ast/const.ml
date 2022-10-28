open Sexplib.Std

type t = Bool of bool | Int of int | Nil [@@deriving sexp]

let equal c1 c2 =
  match (c1, c2) with
  | Bool b1, Bool b2 -> b1 = b2
  | Int n1, Int n2 -> n1 = n2
  | Nil, Nil -> true
  | _ -> false
