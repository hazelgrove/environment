open Sexplib.Std

type node = 
  | PConst of Const.t
  | PVar of Var.t
  | PWild
and t = {
  id : int; (* An unique ID assigned to each node *)
  node : node; (* Node type and its children *)
  starter : bool; (* Whether this is a part of the starter code *)
}
[@@deriving sexp]

type z_t = 
  | Cursor of t
[@@deriving sexp]

type p_t = 
  | Const of Const.t
  | Var of Var.t
  | Wild
[@@deriving sexp]

let equal (p1 : t) (p2 : t) = 
    match p1.node, p2.node with
    | PConst c1, PConst c2 -> Const.equal c1 c2
    | PVar v1, PVar v2 -> Var.equal v1 v2
    | PWild, PWild -> true
    | _ -> false

let z_equal (p1 : z_t) (p2 : z_t) = 
    match p1, p2 with
    | Cursor p1, Cursor p2 -> equal p1 p2

let unzip (zp : z_t) : t = 
    match zp with
    | Cursor p -> p

let select_root (p : t) : z_t = 
    Cursor p

let make_node (node : node) : t = { id = Id.generate (); node; starter = false }

let strip (p : t) : p_t = 
    match p.node with
    | PConst c -> Const c
    | PVar v -> Var v
    | PWild -> Wild

let add_metadata (p : p_t) : t = 
    match p with
    | Const c -> make_node (PConst c)
    | Var v -> make_node (PVar v)
    | Wild -> make_node PWild
