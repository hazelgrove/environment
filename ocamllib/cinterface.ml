open Bigarray
open Astlib

(* Get nodes array from C *)
external get_nodes: unit -> (int, int_elt, c_layout) Array1.t = "pass_nodes" 

(* Pass nodes array to C *)
external pass_nodes: (int, int_elt, c_layout) Array1.t -> unit = "get_nodes"

(* Get edges array from C *)
external get_edges: unit -> (int, int_elt, c_layout) Array2.t = "pass_edges"

(* Pass edges array to C *)
external pass_edges: (int, int_elt, c_layout) Array2.t -> unit = "get_edges"

(* Convert nodes from Bigarray to OCaml list *)
let node_to_list (arr: (int, int_elt, c_layout) Array1.t) (len : int) : int list =
  let rec node_to_list_aux (arr: (int, int_elt, c_layout) Array1.t) (len : int) (l : int list) : int list =
    node_to_list_aux arr (len - 1) (arr.{len - 1} :: list)
  in
  node_to_list_aux arr len []

(* Convert edges from Bigarray to OCaml list *)
let edge_to_list (arr: (int, int_elt, c_layout) Array2.t) (len : int) : (int * int * int) list = 
  let rec edge_to_list_aux (arr: (int, int_elt, c_layout) Array2.t) (len : int) (l : (int * int) list) : (int * int) list =
    edge_to_list_aux arr (len - 1) ((arr.{len - 1, 0}, arr.{len - 1, 1}, arr.{len - 1, 2}) :: list)
  in
  edge_to_list_aux arr len []

(* change_ast function that will be called by C *)
let change_ast_c (action : int) : unit =
  let nodes = get_nodes () in
  let edges = get_edges () in
  let e = c_to_expr nodes in
  let action = tag_to_action action in (* TODO : implement a function with type int -> Action.t *)
  let (nodes, edges) = expr_to_c (change_ast e action) in 
  pass_nodes nodes;;
  pass_edges edges


(* run_unittests function that will be called by C *)
(* TODO: how will the tests be stored? *)
let run_unit_tests_c (test_set : testType list) (code : Expr.t) : unit =
  raise (NotImplemented ())

(* load_assignment function that will be called by C *)
let load_assignment_c (test_num : int) : unit =
  let (nodes, edges) = expr_to_c (load_assignment test_num) in 
  pass_nodes nodes;;
  pass_edges edges

let _ = Callback.register "run_unit_tests" run_unit_tests_c
let _ = Callback.register "change_node" change_node_c
let _ = Callback.register "load_assignment" load_assignment_c