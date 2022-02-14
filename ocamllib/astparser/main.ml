open Ast

(* [subst e1 e2 x] is [e1] with [e2] substituted for [x]. *)
(* let rec subst e1 e2 (x : Var.t) = match e1 with
  | Expr.EVar y      -> if x = y then e2 else e1
  | Expr.EInt c      -> Expr.EInt c
  | Expr.EBool b      -> Expr.EBool b
  | Expr.EBinOp(el, op, er) -> Expr.EBinOp(subst el e2 x, op, subst er e2 x)
  | Expr.EUnOp(op, e) -> Expr.EUnOp(op, subst e e2 x)
  | Expr.ELet(y, el, er) -> if Var.equal x y then Expr.ELet(y, subst el e2 x, er) else Expr.ELet(y, subst el e2 x, subst er e2 x)
  | Expr.EIf(b, el, er) -> Expr.EIf(subst b e2 x, subst el e2 x, subst er e2 x) *)

(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

let read_whole_file filename =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s
  
(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.read lexbuf in
  ast

let parse_file filename =
  let s = read_whole_file filename in
  parse s

