open Sexplib.Std


(* Variables *)
module Var = struct
  type t = string 
  [@@deriving sexp]

  (* need to add in some sort of hole idk how this works *)
  (* Check if two variable identifiers are equal *)
  let equal = String.equal
end

