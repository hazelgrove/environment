open Bigarray
open Ast
open Main

external get_nodes: unit -> (int, int64_elt, c_layout) Array1.t = "pass_nodes"
external pass_nodes: (int, int64_elt, c_layout) Array1.t -> unit = "get_nodes"
external get_edges: unit -> (int, int64_elt, c_layout) Array2.t = "pass_edges"
external pass_edges: (int, int64_elt, c_layout) Array2.t -> unit = "get_edges"

let rec evaluate_ast (e : Expr.t) : Value.t =
  VBool true

let change_ast (action : Action.t) (e : Expr.t) : Expr.t =
  EBool true

let c_to_expr (nodes : (int, int64_elt, c_layout) Array1.t) (edges : (int, int64_elt, c_layout) Array2.t) : Expr.t =
  let rec construct_ast (nodes : (int, int64_elt, c_layout) Array1.t) (curr_node : int) =
    let get_expr (e, _) = e in
    let get_next_node (_, n) = n in

    (* If terminal node *)
    if nodes.{curr_node} = 0 then (EBool true, curr_node + 1)
    else if nodes.{curr_node} = 1 then (EBool false, curr_node + 1)
    else if nodes.{curr_node} = 18 then (EInt -2, curr_node + 1)
    else if nodes.{curr_node} = 19 then (EInt -1, curr_node + 1)
    else if nodes.{curr_node} = 20 then (EInt 0, curr_node + 1)
    else if nodes.{curr_node} = 21 then (EInt 1, curr_node + 1)
    else if nodes.{curr_node} = 22 then (EInt 2, curr_node + 1)
    else if nodes.{curr_node} = 23 then (EVar "x", curr_node + 1)
    else if nodes.{curr_node} = 24 then (EVar "y", curr_node + 1)
    else if nodes.{curr_node} = 25 then (EVar "z", curr_node + 1)

    (* If non-terminal node *)
    else if nodes.{curr_node} = 2 then
      let (e, next) = construct_ast nodes (curr_node + 1) in
      (EUnOp (OpNeg, e), next)
    else if nodes.{curr_node} = 3 then 
      let (e1, next1) = construct_ast nodes (curr_node + 1) in
      let (e2, next2) = construct_ast nodes next1 in
      (EBinOp (e1, OpNeg, e2), next2)
    else
      (EBool true, 1)
  in
    construct_ast nodes 0

let expr_to_c (e : Expr.t) : ((int, int64_elt, c_layout) Array1.t, (int, int64_elt, c_layout) Array2.t) =
  EBool true

let change_ast_c (action : int) : unit =
  let nodes = get_edges () in
  let edges = get_edges () in
  let e = c_to_expr nodes edges in
  let change_node (e : Expr.t) (action : int) -> Expr.t =
    EBool true
  in
  let (nodes, edges) = expr_to_c (change_node e action) in 
  pass_nodes nodes;;
  pass_edges edges

let load_assignment_c (test_num : int) : unit =
  let load_assignment (test_num : int) : Expr.t =
    EBool true
  in
  let (nodes, edges) = expr_to_c (load_assignment test_num) in 
  pass_nodes nodes;;
  pass_edges edges *)

(* let change_node (action : int) : unit =
  let nodes = get_nodes () in
  if nodes.{action} = 0 then 
    (* let () = Array1.set nodes action 1 in pass_nodes nodes *)
    pass_nodes nodes
  else
    (* let () = Array1.set nodes action 0 in pass_nodes nodes *)
    pass_nodes nodes

(* let _ = Callback.register "evaluate_ast" evaluate_ast
let _ = Callback.register "change_node" change_node_c *)