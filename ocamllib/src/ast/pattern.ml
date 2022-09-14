open Sexplib.Std

type t = 
  | PConst of Const.t
  | PVar of Var.t
  | PWild
[@@deriving sexp]

type z_t = 
  | Cursor of t

let equal (p1 : t) (p2 : t) = 
    match p1, p2 with
    | PConst c1, PConst c2 -> Const.equal c1 c2
    | PVar v1, PVar v2 -> Var.equal v1 v2
    | PWild, PWild -> true
    | _ -> false

let z_equal (p1 : z_t) (p2 : z_t) = 
    match p1, p2 with
    | Cursor p1, Cursor p2 -> equal p1 p2
    | _ -> false