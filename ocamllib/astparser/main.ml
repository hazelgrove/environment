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

let c_to_expr (nodes : (int, int_elt, c_layout) Array1.t) : Expr.t =
  let get_expr (e, _) = e in

  let rec construct_ast (nodes : (int, int_elt, c_layout) Array1.t) (curr_node : int) =
    let evar_to_var e = 
      match e with
        | Expr.EVar s -> s
        | _ -> raise (Failure "Incorrect syntax")
    in

    (* Terminal Nodes *)
    if nodes.{curr_node} = 0 then (Expr.EBool true, curr_node + 1)
    else if nodes.{curr_node} = 1 then (Expr.EBool false, curr_node + 1)
    else if nodes.{curr_node} = 18 then (Expr.EInt (-2), curr_node + 1)
    else if nodes.{curr_node} = 19 then (Expr.EInt (-1), curr_node + 1)
    else if nodes.{curr_node} = 20 then (Expr.EInt 0, curr_node + 1)
    else if nodes.{curr_node} = 21 then (Expr.EInt 1, curr_node + 1)
    else if nodes.{curr_node} = 22 then (Expr.EInt 2, curr_node + 1)
    else if nodes.{curr_node} = 23 then (Expr.EVar "x", curr_node + 1)
    else if nodes.{curr_node} = 24 then (Expr.EVar "y", curr_node + 1)
    else if nodes.{curr_node} = 25 then (Expr.EVar "z", curr_node + 1)

    (* Non-Terminal Nodes *)
    else if nodes.{curr_node} = 2 then
      let (e, next) = construct_ast nodes (curr_node + 1) in
      (Expr.EUnOp (Expr.OpNeg, e), next)
    else if nodes.{curr_node} > 2 && nodes.{curr_node} < 15 then 
      let (e1, next1) = construct_ast nodes (curr_node + 1) in
      let (e2, next2) = construct_ast nodes next1 in
      let op = 
        (match nodes.{curr_node} with 
          | 3 -> Expr.OpPlus
          | 4 -> Expr.OpMinus
          | 5 -> Expr.OpTimes
          | 6 -> Expr.OpDiv
          | 7 -> Expr.OpLt
          | 8 -> Expr.OpLe
          | 9 -> Expr.OpGt
          | 10 -> Expr.OpGe
          | 11 -> Expr.OpEq
          | 12 -> Expr.OpNe
          | 13 -> Expr.OpCon
          | 14 -> Expr.OpAp
          | _ -> raise (Failure "Incorrect syntax")
        )
      in
      (Expr.EBinOp (e1, op, e2), next2)
    else if nodes.{curr_node} = 15 then
      let (e1, next1) = construct_ast nodes (curr_node + 2) in
      let (e2, next2) = construct_ast nodes next1 in
      (Expr.ELet (evar_to_var (get_expr (construct_ast nodes (curr_node + 1))), e1, e2), next2)
    else if nodes.{curr_node} = 16 then
      let (econd, next_cond) = construct_ast nodes (curr_node + 1) in
      let (ethen, next_then) = construct_ast nodes next_cond in
      let (eelse, next_else) = construct_ast nodes next_then in
      (Expr.EIf (econd, ethen, eelse), next_else)
    else if nodes.{curr_node} = 17 then
      let (e, next) = construct_ast nodes (curr_node + 2) in
      (Expr.EFun (evar_to_var (get_expr (construct_ast nodes (curr_node + 1))), e), next)
    else
      (Expr.EBool true, 1)
  in
    get_expr (construct_ast nodes 0)



let expr_to_c (e : Expr.t) : ((int, int64_elt, c_layout) Array1.t, (int, int64_elt, c_layout) Array2.t) =
  EBool true
