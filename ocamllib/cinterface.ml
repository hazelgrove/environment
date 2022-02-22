open Bigarray
open Astlib

external get_nodes: unit -> (int, int_elt, c_layout) Array1.t = "pass_nodes"
external pass_nodes: (int, int_elt, c_layout) Array1.t -> unit = "get_nodes"
external get_edges: unit -> (int, int_elt, c_layout) Array2.t = "pass_edges"
external pass_edges: (int, int_elt, c_layout) Array2.t -> unit = "get_edges"

let node_to_list (arr: (int, int_elt, c_layout) Array1.t) (len : int) : int list =
  let rec node_to_list_aux (arr: (int, int_elt, c_layout) Array1.t) (len : int) (l : int list) : int list =
    node_to_list_aux arr (len - 1) (arr.{len - 1} :: list)
  in
  node_to_list_aux arr len []

let edge_to_list (arr: (int, int_elt, c_layout) Array1.t) (len : int) : (int * int * int) list = 
  let rec edge_to_list_aux (arr: (int, int_elt, c_layout) Array1.t) (len : int) (l : (int * int) list) : (int * int) list =
    edge_to_list_aux arr (len - 1) ((arr.{len - 1, 0}, arr.{len - 1, 1}, arr.{len - 1, 2}) :: list)
  in
  edge_to_list_aux arr len []

let change_ast_c (action : Action.t) : unit =
  let nodes = get_nodes () in
  let edges = get_edges () in
  let e = c_to_expr nodes in
  
  let (nodes, edges) = expr_to_c (change_ast e action) in 
  pass_nodes nodes;;
  pass_edges edges



let load_assignment_c (test_num : int) : unit =
  
  let (nodes, edges) = expr_to_c (load_assignment test_num) in 
  pass_nodes nodes;;
  pass_edges edges

let _ = Callback.register "evaluate_ast" evaluate_ast
let _ = Callback.register "change_node" change_node_c