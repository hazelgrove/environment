module Typ = struct
  type t =
    | Int
    | Bool
    | Arrow of t * t

  let rec equal ty ty' =
    match (ty, ty') with
    | (Int, Int)
    | (Bool, Bool) -> true
    | (Arrow (tin1, tout1), Arrow (tin2, tout2)) -> (equal tin1 tin2) && (equal tout1 tout2)
    | _ -> false
end

module Var = struct
  type t = string

  let equal = String.equal
end

module Expr = struct
  type unop = OpNeg
  type binop = OpPlus | OpMinus | OpTimes | OpLt | OpLe | OpGt | OpGe | OpEq | OpNe | OpCon | OpAp

  type t =
    | EVar of Var.t
    | EInt of int
    | EBool of bool
    | EUnOp of unop * t
    | EBinOp of t * binop * t
    | ELet of Var.t * t * t
    | EIf of t * t * t
    | EFun of Var.t * t
end

module Value = struct
  type t = 
    | VInt of int
    | VBool of bool
    | VFun of (Var.t * Expr.t)
end