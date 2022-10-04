open Sexplib.Std

(* Variables *)
type t = int [@@deriving sexp]

let max_num_vars : int = 10
let num_vars : int ref = ref 0
let used_vars : bool Array.t = Array.make max_num_vars false

(* Definition of special varaibles *)
let starter_func : t = max_num_vars
let undef_var : t = -2

let get_new_var _ : t =
  let new_var =
    let rec find n =
      if n < max_num_vars
      then if used_vars.(n) then find (n + 1) else n
      else raise (Failure "No free variables")
    in
    find 0
  in
  used_vars.(new_var) <- true;
  num_vars := !num_vars + 1;
  new_var

let free_var (x : t) : unit =
  used_vars.(x) <- false;
  num_vars := !num_vars - 1

let reset _ : unit =
  num_vars := 0;
  Array.fill used_vars 0 max_num_vars false

(* Check if two variable identifiers are equal *)
let equal (x1 : t) (x2 : t) : bool = x1 = x2

let to_string (var : t) =
  if equal var starter_func then "f" else "x" ^ string_of_int var

let is_normal (var : t) = if var >= max_num_vars || var < 0 then false else true
