open Sexplib.Std

(* Basic types *)
module Typ = struct
  type t =
    | Int
    | Bool
    | Arrow of t * t

  (* Check if two types are equal *)
  let rec equal (ty : t) (ty' : t) : bool =
    match (ty, ty') with
    | (Int, Int)
    | (Bool, Bool) -> true
    | (Arrow (tin1, tout1), Arrow (tin2, tout2)) -> (equal tin1 tin2) && (equal tout1 tout2)
    | _ -> false
end

(* Variables *)
module Var = struct
  type t = string [@@deriving sexp]
  (* need to add in some sort of hole idk how this works *)
  (* Check if two variable identifiers are equal *)
  let equal = String.equal
end

(* AST Definition *)
module Expr = struct
  type unop = OpNeg [@@deriving sexp]
  type binop = OpPlus | OpMinus | OpTimes | OpDiv | OpLt | OpLe | OpGt | OpGe | OpEq | OpNe | OpCon | OpAp [@@deriving sexp]

  type t =
    | EVar of Var.t                         (* Node Descriptor Number : 35 - 37 *)
    | EInt of int                           (* Node Descriptor Number : 30 - 34 *)
    | EBool of bool                         (* Node Descriptor Number : 0 - 1 *)
    | EUnOp of unop * t                     (* Node Descriptor Number : 2 *)
    | EBinOp of t * binop * t               (* Node Descriptor Number : 3 - 14 *)
    | ELet of Var.t * t * t                 (* Node Descriptor Number : 15 *)
    | EIf of t * t * t                      (* Node Descriptor Number : 16 *)
    | EFun of Var.t * t                     (* Node Descriptor Number : 17 *)
    | EFix of Var.t * t                     (* Node Descriptor Number : 18 *)
    | EPair of t * t
    | EHole                                 (* Node Descriptor Number : 19 *)
    | ENil
  [@@deriving sexp]

  type z_t = 
    | Cursor of t
    | EUnOp_L of unop * z_t
    | EBinOp_L of z_t * binop * t 
    | EBinOp_R of t * binop * z_t 
    | ELet_L of Var.t * z_t *t 
    | ELet_R of Var.t * t * z_t
    | EIf_L of z_t * t * t
    | EIf_C of t * z_t * t
    | EIf_R of t * t * z_t
    | EFun_L of Var.t * z_t 
    | EFix_L   of Var.t * z_t 
    | EPair_L of z_t * t
    | EPair_R of t * z_t
    [@@deriving sexp]

  type tag = int

  let node_to_tag (node : t) : tag = 
    match node with
      | EUnOp (OpNeg, _) -> 0
      | EBinOp (_, op, _) ->
        (match op with
          | OpPlus -> 1
          | OpMinus -> 2
          | OpTimes -> 3
          | OpDiv -> 4
          | OpLt -> 5
          | OpLe -> 6
          | OpGt -> 7
          | OpGe -> 8
          | OpEq -> 9
          | OpNe -> 10
          | OpCon -> 11
          | OpAp -> 12
        )
      | ELet (_, _, _) -> 13
      | EIf (_, _, _) -> 14
      | EFun (_, _) -> 15
      | EFix (_, _) -> 16
      | EPair (_, _) -> 17
      | EHole -> 30
      | EBool false -> 31
      | EBool true -> 32
      | EInt (-2) -> 33
      | EInt (-1) -> 34
      | EInt 0 -> 35
      | EInt 1 -> 36
      | EInt 2 -> 37
      | EVar "x" -> 38
      | EVar "y" -> 39
      | EVar "z" -> 40
      | ENil -> 41
      | _ -> raise (Failure "Not supported yet")

  let tag_to_node (tag : tag) (child1 : t option) (child2 : t option) (child3 : t option) : t = 
    let check_child (child : t option) : t =
      match child with
        | Some e -> e
        | _ -> raise (Failure "Incorrect syntax")
    in
    let expr_to_var (e : t) : Var.t =
      match e with
        | EVar s -> s
        | _ -> raise (Failure "Incorrect syntax")
    in
    match tag with
      | 0 -> EUnOp (OpNeg, check_child child1)
      | 1 -> EBinOp (check_child child1, OpPlus, check_child child2)
      | 2 -> EBinOp (check_child child1, OpMinus, check_child child2)
      | 3 -> EBinOp (check_child child1, OpTimes, check_child child2)
      | 4 -> EBinOp (check_child child1, OpDiv, check_child child2)
      | 5 -> EBinOp (check_child child1, OpLt, check_child child2)
      | 6 -> EBinOp (check_child child1, OpLe, check_child child2)
      | 7 -> EBinOp (check_child child1, OpGt, check_child child2)
      | 8 -> EBinOp (check_child child1, OpGe, check_child child2)
      | 9 -> EBinOp (check_child child1, OpEq, check_child child2)
      | 10 -> EBinOp (check_child child1, OpNe, check_child child2)
      | 11 -> EBinOp (check_child child1, OpCon, check_child child2)
      | 12 -> EBinOp (check_child child1, OpAp, check_child child2)
      | 13 -> ELet (expr_to_var (check_child child1), check_child child2, check_child child3)
      | 14 -> EIf (check_child child1, check_child child2, check_child child3)
      | 15 -> EFun (expr_to_var (check_child child1), check_child child2)
      | 16 -> EFix (expr_to_var (check_child child1), check_child child2)
      | 17 -> EPair (check_child child1, check_child child2)
      | 30 -> EHole
      | 31 -> EBool false
      | 32 -> EBool true
      | 33 -> EInt (-2)
      | 34 -> EInt (-1)
      | 35 -> EInt 0
      | 36 -> EInt 1
      | 37 -> EInt 2
      | 38 -> EVar "x"
      | 39 -> EVar "y"
      | 40 -> EVar "z"
      | 41 -> ENil
      | _ -> raise (Failure "Not supported")

  let tag_to_word (tag : tag) : string = 
    match tag with
      | 0 -> "-"
      | 1 -> "+"
      | 2 -> "-"
      | 3 -> "*"
      | 4 -> "/"
      | 5 -> "<"
      | 6 -> "<="
      | 7 -> ">"
      | 8 -> ">="
      | 9 -> "="
      | 10 -> "!="
      | 11 -> "::"
      | 12 -> "(ap)"
      | 13 -> "let"
      | 14 -> "if"
      | 15 -> "fun"
      | 16 -> "fix"
      | 17 -> "pair"
      | 30 -> "hole"
      | 31 -> "false"
      | 32 -> "true"
      | 33 -> "-2"
      | 34 -> "-1"
      | 35 -> "0"
      | 36 -> "1"
      | 37 -> "2"
      | 38 -> "x"
      | 39 -> "y"
      | 40 -> "z"
      | 41 -> "[]"
      | _ -> raise (Failure "Not supported")
end

(* Values *)
module Value = struct
  type t = 
    | VInt of int
    | VBool of bool
    | VFun of Var.t * Expr.t
    | VPair of t * t
    | VNil
  
  let rec to_expr (v : t) =
    match v with
      | VInt n -> Expr.EInt n
      | VBool b -> Expr.EBool b
      | VFun (x, e) -> Expr.EFun (x, e)
      | VPair (e1, e2) -> Expr.EPair (to_expr e1, to_expr e2)
      | VNil -> ENil
end

module Action = struct
  type shape = 
    | Var of Var.t
    | Hole 
    | Nil
    | Int of int 
    | Bool of bool 
    | UnOp of Expr.unop 
    | BinOp_L of Expr.binop 
    | BinOp_R of Expr.binop 
    | Let_L of Var.t 
    | Let_R of Var.t
    | If_L  
    | If_C 
    | If_R 
    | Fun   of Var.t
    | Fix   of Var.t
    | Pair_L 
    | Pair_R
  

  
  type dir = 
    | Parent
    | Child of int

    (* write a numbered action to inser all of <- *)
    (* Have some sort of default value analog for type t *)
    (* Look at only allowing inserts on empty holes... *)
    (* maybe have delete move the subtree into 'copy' register *)
    
  type t = 
    (*| Del                     (* Action Number: 0 *)
    (* | Finish                  Action Number: 1 *)*)
    | Move of dir             (* Action Number: 2-5 *)
    | Construct of shape      (* Action Number: 6- (36 ish) *)

  type tag = int

  type avail_actions = {
    move_parent: bool; 
    max_child: int;
    in_scope: bool list   
  }
  (*  Contains short-form avaliable actions*)
  (*  In the format (Parent avaliable, 
                    max child number (if 0 no children exist),
                    can_construct?
                    A list of 10 bools indicating if variables 'v0' ... 'v9' have been seen )*)


  let tag_to_action (action : tag) = 
    let _ = action in Move Parent
end
