open Ast
open Bigarray

(* [subst e1 e2 x] is [e1] with [e2] substituted for [x]. *)
(* let rec subst e1 e2 (x : Var.t) = match e1 with
  | Expr.EVar y      -> if x = y then e2 else e1
  | Expr.EInt c      -> Expr.EInt c
  | Expr.EBool b      -> Expr.EBool b
  | Expr.EBinOp(el, op, er) -> Expr.EBinOp(subst el e2 x, op, subst er e2 x)
  | Expr.EUnOp(op, e) -> Expr.EUnOp(op, subst e e2 x)
  | Expr.ELet(y, el, er) -> if Var.equal x y then Expr.ELet(y, subst el e2 x, er) else Expr.ELet(y, subst el e2 x, subst er e2 x)
  | Expr.EIf(b, el, er) -> Expr.EIf(subst b e2 x, subst el e2 x, subst er e2 x) *)
  
(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast

(* Parse a file (assuming it is a well-typed .ml file) into an ast *)
let parse_file filename =
  let read_whole_file filename =
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    s
  in
  let s = read_whole_file filename in
  parse s

type edge = (int * int * int)
type node = int
type graph = (node list) * (edge list)

let get_adj_nodes (edges : edge list) (start_node : int) : edge list =
  List.filter (fun (start, _, _) -> start = start_node) edges

let rec c_to_expr (nodes : node list) (edges : edge list) (curr_node : int) : Expr.t =
  let tag = List.nth nodes curr_node in
  if tag >= 30 then Tag.tag_to_node tag None None None else
  let adj_nodes = get_adj_nodes edges curr_node in
  let get_nth_child n = 
    let nth_child = List.filter (fun (_, _, child_num) -> child_num = n) adj_nodes in
    match nth_child with
    | [] -> None
    | [(_, endpt, _)] -> Some (c_to_expr nodes edges endpt)
    | _ -> raise (Failure "Invalid syntax")
  in
  Tag.tag_to_node tag (get_nth_child 1) (get_nth_child 2) (get_nth_child 3)

let expr_to_c (e : Expr.t) : (graph * int) = 
  let add_node (nodes : node list) (tag : Tag.t) : (node list * int) =
    let new_nodes = nodes @ [tag] in (new_nodes, List.length nodes)
  in
  let add_edge (edges : edge list) (new_edge : edge) : (edge list) =
    new_edge :: edges
  in
  let rec expr_to_c_aux (e : Expr.t) (nodes : node list) (edges : edge list) : (graph * int) = 
    let tag = Tag.node_to_tag e in
    if tag >= 30 then 
      let (nodes, root) = add_node nodes tag in ((nodes, edges), root)
    else 
      let add_unary subexpr = 
        let ((nodes, edges), root) = expr_to_c_aux subexpr nodes edges in
        let (nodes, new_root) = add_node nodes tag in
        let edges = add_edge edges (new_root, root, 1) in
        ((nodes, edges), new_root)
      in
      let add_binary sub1 sub2 = 
        let ((nodes, edges), root1) = expr_to_c_aux sub1 nodes edges in
        let ((nodes, edges), root2) = expr_to_c_aux sub2 nodes edges in
        let (nodes, new_root) = add_node nodes tag in
        let edges = add_edge edges (new_root, root1, 1) in
        let edges = add_edge edges (new_root, root2, 2) in
        ((nodes, edges), new_root)
      in
      let add_ternary sub1 sub2 sub3 = 
        let ((nodes, edges), root1) = expr_to_c_aux sub1 nodes edges in
        let ((nodes, edges), root2) = expr_to_c_aux sub2 nodes edges in
        let ((nodes, edges), root3) = expr_to_c_aux sub3 nodes edges in
        let (nodes, new_root) = add_node nodes tag in
        let edges = add_edge edges (new_root, root1, 1) in
        let edges = add_edge edges (new_root, root2, 2) in
        let edges = add_edge edges (new_root, root3, 3) in
        ((nodes, edges), new_root)
      in
      match e with
        | EUnOp (_, subexpr) -> add_unary subexpr
        | EBinOp (sub1, _ , sub2) -> add_binary sub1 sub2
        | ELet (sub1, sub2, sub3) -> add_ternary (EVar sub1) sub2 sub3
        | EIf (sub1, sub2, sub3) -> add_ternary sub1 sub2 sub3
        | EFun (sub1, sub2) -> add_binary (EVar sub1) sub2
        | EFix (sub1, sub2) -> add_binary (EVar sub1) sub2
        | _ -> raise (Failure "Incorrect syntax")
    in
      expr_to_c_aux e [] []