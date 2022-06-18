open Sexplib.Std

(* Variables *)
type t = string [@@deriving sexp]

let undef_var = ""

(* need to add in some sort of hole idk how this works *)
(* Check if two variable identifiers are equal *)
let equal = String.equal
let to_string (var : t) = var
