open Sexplib.Std

(* Variables *)
type t = int [@@deriving sexp]

let max_num_vars : int = 10
let used_vars : bool Array.t = Array.make max_num_vars false

let undef_var : t = -1

let get_new_var _ : t =
  let new_var = 
    let rec find n = 
      if n < max_num_vars then
        if used_vars.(n) then
          find (n + 1)
        else
          n
      else
        raise (Failure "No free variables")
    in
    find 0
  in
  used_vars.(new_var) <- true;
  new_var

let free_var (x : t) : unit = 
  used_vars.(x) <- false

(* Check if two variable identifiers are equal *)
let equal (x1 : t) (x2 : t) : bool = x1 = x2
let to_string (var : t) = "x" ^ string_of_int var
