open Sexplib.Std

(* Variables *)
type t = int [@@deriving sexp]

let undef_var : t = -1
let last_var : t ref = ref undef_var

let get_new_var _ : t =
  last_var := !last_var + 1;
  !last_var

let reset : unit = last_var := undef_var

(* Check if two variable identifiers are equal *)
let equal (x1 : t) (x2 : t) : bool = x1 = x2
let to_string (var : t) = "x" ^ string_of_int var
