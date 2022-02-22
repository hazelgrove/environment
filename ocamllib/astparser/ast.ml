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
  type t = string

  (* Check if two variable identifiers are equal *)
  let equal = String.equal
end

(* AST Definition *)
module Expr = struct
  type unop = OpNeg
  type binop = OpPlus | OpMinus | OpTimes | OpDiv | OpLt | OpLe | OpGt | OpGe | OpEq | OpNe | OpCon | OpAp

  type t =
    | EVar of Var.t                         
    | EInt of int   
    | EBool of bool 
    | EUnOp of unop * t 
    | EBinOp of t * binop * t
    | ELet of Var.t * t * t
    | EIf of t * t * t 
    | EFun of Var.t * t
    | EFix of Var.t * t
    | EPair of t * t
    | EHole
    | ENil
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

(* Actions as defined in Hazel paper *)
module Action = struct
  type dir = 
    | Child of int
    | Parent

  type shape =
    | Arrow
    | Num
    | Asc
    | Var of Var.t
    | Lam of Var.t
    | Ap
    | Lit of int
    | Plus

  type t = 
    (* | Del                     (* Action Number: 0 *)
    | Finish                  Action Number: 1 *)
    | Move of dir             (* Action Number: 2-5 *)
    | Construct of shape      (* Action Number: 6-14 *)
end

module Tag = struct
  type t = int

  let node_to_tag (node : Expr.t) : t = 
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

  let tag_to_node (tag : t) (child1 : Expr.t option) (child2 : Expr.t option) (child3 : Expr.t option) : Expr.t = 
    let check_child (child : Expr.t option) : Expr.t =
      match child with
        | Some e -> e
        | _ -> raise (Failure "Incorrect syntax")
    in
    let expr_to_var (e : Expr.t) : Var.t =
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
end