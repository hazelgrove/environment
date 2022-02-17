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
    | EVar of Var.t                 (* Node Descriptor Number : 23 - 25 *)
    | EInt of int                   (* Node Descriptor Number : 18 - 22 *)
    | EBool of bool                 (* Node Descriptor Number : 0 - 1 *)
    | EUnOp of unop * t             (* Node Descriptor Number : 2 *)
    | EBinOp of t * binop * t       (* Node Descriptor Number : 3 - 14 *)
    | ELet of Var.t * t * t         (* Node Descriptor Number : 15 *)
    | EIf of t * t * t              (* Node Descriptor Number : 16 *)
    | EFun of Var.t * t             (* Node Descriptor Number : 17 *)
end

(* Values *)
module Value = struct
  type t = 
    | VInt of int
    | VBool of bool
    | VFun of (Var.t * Expr.t)
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
    | Nehole

  type t = 
    | Del                     (* Action Number: 0 *)
    | Finish                  (* Action Number: 1 *)
    | Move of dir             (* Action Number: 2-5 *)
    | Construct of shape      (* Action Number: 6-14 *)
end

module Tag = struct
  type t = int

  let node_to_tag (node : Expr.t) : t = 
    match node with
      | EBool false -> 0
      | EBool true -> 1
      | EUnOp (OpNeg, _) -> 2
      | EBinOp (_, op, _) ->
        (match op with
          | OpPlus -> 3
          | OpMinus -> 4
          | OpTimes -> 5
          | OpDiv -> 6
          | OpLt -> 7
          | OpLe -> 8
          | OpGt -> 9
          | OpGe -> 10
          | OpEq -> 11
          | OpNe -> 12
          | OpCon -> 13
          | OpAp -> 14
        )
      | ELet (_, _, _) -> 15
      | EIf (_, _, _) -> 16
      | EFun (_, _, _) -> 17
      | EVar "x" -> 18
      | EVar "y" -> 19
      | EVar "z" -> 20
      | EInt (-2) -> 21
      | EInt (-1) -> 22
      | EInt 0 -> 23
      | EInt 1 -> 24
      | EInt 2 -> 25
end