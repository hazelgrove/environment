open Sexplib.Std

type node = 
  | PConst of Const.t
  | PVar of Var.t
  | PCons of t * t
  | PWild
and t = {
  id : int; (* An unique ID assigned to each node *)
  node : node; (* Node type and its children *)
  starter : bool; (* Whether this is a part of the starter code *)
}
[@@deriving sexp]

type z_node = 
  | Cursor of node
  | PCons_L of z_t * t
  | PCons_R of t * z_t
and z_t = {
  id : int; (* An unique ID assigned to each node *)
  node : z_node; (* Node type and its children *)
  starter : bool; (* Whether this is a part of the starter code *)
}
[@@deriving sexp]

type p_t = 
  | Const of Const.t
  | Var of Var.t
  | Cons of p_t * p_t
  | Wild
[@@deriving sexp]

let make_node (node : node) : t = { id = Id.generate (); node; starter = false }
let make_dummy_node (node : node) : t = { id = -1; node; starter = false }

let select_root (p : t) : z_t =
  { id = p.id; node = Cursor p.node; starter = p.starter }

let zip_migrate (p : t) (node : z_node) : z_t =
  { id = p.id; node; starter = p.starter }

let rec equal (p1 : t) (p2 : t) = 
    match p1.node, p2.node with
    | PConst c1, PConst c2 -> Const.equal c1 c2
    | PVar v1, PVar v2 -> Var.equal v1 v2
    | PWild, PWild -> true
    | PCons (p1, p2), PCons (p1', p2') -> equal p1 p1' && equal p2 p2'
    | _ -> false

let rec z_equal (p1 : z_t) (p2 : z_t) = 
    match p1.node, p2.node with
    | Cursor p1, Cursor p2 -> equal (make_dummy_node p1) (make_dummy_node p2)
    | PCons_L (zp1, p1), PCons_L (zp2, p2)
    | PCons_R (p1, zp1), PCons_R (p2, zp2) ->
        z_equal zp1 zp2 && equal p1 p2
    | _ -> false

let rec unzip (zp : z_t) : t = 
    let node : node =
      match zp.node with
      | Cursor p -> p
      | PCons_L (zp, p) -> PCons (unzip zp, p)
      | PCons_R (p, zp) -> PCons (p, unzip zp)
    in
    { id = zp.id; node; starter = zp.starter }

let rec strip (p : t) : p_t = 
    match p.node with
    | PConst c -> Const c
    | PVar v -> Var v
    | PCons (p1, p2) -> Cons (strip p1, strip p2)
    | PWild -> Wild

let rec add_metadata (p : p_t) : t = 
    match p with
    | Const c -> make_node (PConst c)
    | Var v -> make_node (PVar v)
    | Cons (p1, p2) -> make_node (PCons (add_metadata p1, add_metadata p2))
    | Wild -> make_node PWild

let rec size (p : t) : int = 
    match p.node with
    | PConst _ | PVar _ | PWild -> 1
    | PCons (p1, p2) -> 1 + size p1 + size p2

let rec set_starter (p : t) (b : bool) : t = 
    let new_node =
        match p.node with
        | PVar _ | PConst _ | PWild -> p.node
        | PCons (p1, p2) -> PCons (set_starter p1 b, set_starter p2 b)
    in
    { p with node = new_node; starter = b }
