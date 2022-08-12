(* Defines actions that can be performed by the agent *)

type shape =
  | Var of Var.t
  | Hole
  | Nil
  | Int of int
  | Bool of bool
  | UnOp of Expr.unop
  | BinOp_L of Expr.binop
  | BinOp_R of Expr.binop
  | Let_L
  | Let_R
  | If_L
  | If_C
  | If_R
  | Fun
  | Fix
  | Pair_L
  | Pair_R
  | TypInt
  | TypBool
  | TypArrow_L
  | TypArrow_R
  | TypList (*beacause there's only one child no need for option*)
  | TypHole
  | TypProd_L
  | TypProd_R

type dir = Parent | Child of int

(* write a numbered action to inser all of <- *)
(* Have some sort of default value analog for type t *)
(* Look at only allowing inserts on empty holes... *)
(* maybe have delete move the subtree into 'copy' register *)

type t = Move of dir | Construct of shape | Unwrap of int

(*  Contains short-form avaliable actions*)
(* In the format (Parent avaliable,
                  max child number (if 0 no children exist),
                  can_construct?
                  A list of 10 bools indicating if variables 'v0' ... 'v9' have been seen )*)
