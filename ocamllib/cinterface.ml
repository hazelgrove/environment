open Bigarray
open Astlib
open Astutil
open Ast

(* Get nodes array from C *)
external get_nodes: unit -> (int, int_elt, c_layout) Array1.t = "pass_nodes" 

(* Pass nodes array to C *)
external pass_nodes: (int, int_elt, c_layout) Array1.t -> unit = "get_nodes"

(* Get edges array from C *)
external get_edges: unit -> (int, int_elt, c_layout) Array2.t = "pass_edges"

(* Pass edges array to C *)
external pass_edges: (int, int_elt, c_layout) Array2.t -> unit = "get_edges"

(* Convert nodes from Bigarray to OCaml list *)
let node_to_list (arr: (int, int_elt, c_layout) Array1.t) : int list =
  let rec node_to_list_aux (arr: (int, int_elt, c_layout) Array1.t) (len : int) : int list = 
    if len > 0 then
      arr.{len - 1} :: node_to_list_aux arr (len - 1)
    else
      []
  in
  node_to_list_aux arr (Array1.dim arr)

(* Convert OCaml nodes list to Bigarray *)
let list_to_node (l : int list) : (int, int_elt, c_layout) Array1.t = 
  let nodes = Array1.create Int c_layout (List.length l) in
  let rec list_to_node_aux (l : int list) (arr : (int, int_elt, c_layout) Array1.t) (count : int) : unit = 
    match l with
      | [] -> ()
      | hd :: tl ->
        let _ = arr.{count} <- hd in 
        list_to_node_aux tl arr (count + 1)
  in
  let _ = list_to_node_aux l nodes 0 in
  nodes

(* Convert edges from Bigarray to OCaml list *)
let edge_to_list (arr: (int, int_elt, c_layout) Array2.t) : (int * int * int) list = 
  let rec edge_to_list_aux (arr: (int, int_elt, c_layout) Array2.t) (len : int) : (int * int * int) list = 
    if len > 0 then
      (arr.{len - 1, 0}, arr.{len - 1, 1}, arr.{len - 1, 2}) :: edge_to_list_aux arr (len - 1)
    else
      []
  in
  edge_to_list_aux arr (Array2.dim1 arr)

(* Convert OCaml edges list to Bigarray *)
let list_to_edge (l : (int * int * int) list) : (int, int_elt, c_layout) Array2.t = 
  let edges = Array2.create Int c_layout (List.length l) 3 in
  let rec list_to_edge_aux (l : (int * int * int) list) (arr : (int, int_elt, c_layout) Array2.t) (count : int) : unit = 
    match l with
      | [] -> ()
      | (start, stop, index) :: tl -> 
        let _ = arr.{count, 0} <- start in
        let _ = arr.{count, 1} <- stop in
        let _ = arr.{count, 2} <- index in
        list_to_edge_aux tl arr (count + 1)
  in
  let _ = list_to_edge_aux l edges 0 in
  edges

(* change_ast function that will be called by C *)
let change_ast_c (root : int) (action : int) : unit =
  let nodes = node_to_list (get_nodes ()) in
  let edges = edge_to_list (get_edges ()) in
  let e = c_to_expr nodes edges root in
  let action = Action.tag_to_action action in
  let ((nodes, edges), root) = expr_to_c (change_ast e action) in (* TODO : implement a function with type int -> Action.t *)
  let _ = pass_nodes (list_to_node nodes) in
  pass_edges (list_to_edge edges)


(* run_unittests function that will be called by C *)
(* TODO: how will the tests be stored? *)
let run_unit_tests_c (test_set : testType list) (code : Expr.t) : unit =
  raise (NotImplemented)

(* load_assignment function that will be called by C *)
let load_assignment_c (directory : string) (assignment : int) (num_files : int) : unit =
  let (_, _) = load_assignment directory assignment num_files in
  raise (NotImplemented)

let _ = Callback.register "run_unit_tests" run_unit_tests_c
let _ = Callback.register "change_ast" change_ast_c
let _ = Callback.register "load_assignment" load_assignment_c