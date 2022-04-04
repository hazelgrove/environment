open Type
open Sexplib.Std

(* Variables *)
module Var = struct
  type t = string [@@deriving sexp]

  (* need to add in some sort of hole idk how this works *)
  (* Check if two variable identifiers are equal *)
  let equal = String.equal
end

module Assumptions = struct
  type assumption = Var.t * Typ.t
  type t = assumption list

  let empty : t = []

  let lookup (ctx : t) (x : Var.t) : Typ.t option =
    List.fold_left
      (fun found (y, ty) ->
        match found with
        | Some _ -> found
        | None -> if Var.equal x y then Some ty else None)
      None ctx

  let extend (ctx : t) ((x, ty) : assumption) : t =
    match lookup ctx x with
    | None -> (x, ty) :: ctx
    | Some _ ->
        List.fold_right
          (fun (y, ty') new_ctx ->
            let ty = if Var.equal x y then ty else ty' in
            (y, ty) :: new_ctx)
          ctx empty
end

(* AST Definition *)
module Expr = struct
  type unop = OpNeg [@@deriving sexp]

  type binop =
    | OpPlus
    | OpMinus
    | OpTimes
    | OpDiv
    | OpLt
    | OpLe
    | OpGt
    | OpGe
    | OpEq
    | OpNe
    | OpCon
    | OpAp
  [@@deriving sexp]

  type t =
    | EVar of Var.t (* Node Descriptor Number : 35 - 37 *)
    | EInt of int (* Node Descriptor Number : 30 - 34 *)
    | EBool of bool (* Node Descriptor Number : 0 - 1 *)
    | EUnOp of unop * t (* Node Descriptor Number : 2 *)
    | EBinOp of t * binop * t (* Node Descriptor Number : 3 - 14 *)
    | ELet of Var.t * t * t (* Node Descriptor Number : 15 *)
    | EIf of t * t * t (* Node Descriptor Number : 16 *)
    | EFun of Var.t * Typ.t * t (* Node Descriptor Number : 17 *)
    | EFix of Var.t * Typ.t * t (* Node Descriptor Number : 18 *)
    | EPair of t * t
    | EHole (* Node Descriptor Number : 19 *)
    | ENil
  [@@deriving sexp]

  type z_t =
    | Cursor of t
    | EUnOp_L of unop * z_t
    | EBinOp_L of z_t * binop * t
    | EBinOp_R of t * binop * z_t
    | ELet_L of Var.t * z_t * t
    | ELet_R of Var.t * t * z_t
    | EIf_L of z_t * t * t
    | EIf_C of t * z_t * t
    | EIf_R of t * t * z_t
    | EFun_L of Var.t * Typ.t * z_t
    | EFun_T of Var.t * Typ.t * t (* TOOD: need to fix all our rerucsion operations now *)
    | EFix_L of Var.t * Typ.t * z_t
    | EFix_T of Var.t * Typ.t * t (* TOOD: need to fix all our rerucsion operations now *)
    | EPair_L of z_t * t
    | EPair_R of t * z_t
  [@@deriving sexp]

  type value =
    | VInt of int
    | VBool of bool
    | VFun of Var.t * Typ.t * t
    | VPair of value * value
    | VNil
    | VError

  type tag = int

  let node_to_tag (node : t) : tag =
    match node with
    | EUnOp (OpNeg, _) -> 0
    | EBinOp (_, op, _) -> (
        match op with
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
        | OpAp -> 12)
    | ELet (_, _, _) -> 13
    | EIf (_, _, _) -> 14
    | EFun (_, _, _) -> 15
    | EFix (_, _, _) -> 16
    | EPair (_, _) -> 17
    | EHole -> 30
    | EBool false -> 31
    | EBool true -> 32
    | EInt -2 -> 33
    | EInt -1 -> 34
    | EInt 0 -> 35
    | EInt 1 -> 36
    | EInt 2 -> 37
    | EVar "x" -> 38
    | EVar "y" -> 39
    | EVar "z" -> 40
    | ENil -> 41
    | _ -> raise (Failure "Not supported yet")

  let tag_to_node (tag : tag) : t =
    match tag with
    | 0 -> EUnOp (OpNeg, EHole)
    | 1 -> EBinOp (EHole, OpPlus, EHole)
    | 2 -> EBinOp (EHole, OpMinus, EHole)
    | 3 -> EBinOp (EHole, OpTimes, EHole)
    | 4 -> EBinOp (EHole, OpDiv, EHole)
    | 5 -> EBinOp (EHole, OpLt, EHole)
    | 6 -> EBinOp (EHole, OpLe, EHole)
    | 7 -> EBinOp (EHole, OpGt, EHole)
    | 8 -> EBinOp (EHole, OpGe, EHole)
    | 9 -> EBinOp (EHole, OpEq, EHole)
    | 10 -> EBinOp (EHole, OpNe, EHole)
    | 11 -> EBinOp (EHole, OpCon, EHole)
    | 12 -> EBinOp (EHole, OpAp, EHole)
    | 13 -> ELet ("", EHole, EHole)
    | 14 -> EIf (EHole, EHole, EHole)
    | 15 -> EFun ("", THole, EHole)
    | 16 -> EFix ("", THole, EHole)
    | 17 -> EPair (EHole, EHole)
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

  (*
     Return the size of the AST
     Input :
       - e : the AST
     Output :
       - the size of the AST
  *)
  let rec size (e : t) : int =
    match e with
    | EVar _ | EInt _ | EBool _ | EHole | ENil -> 1
    | EUnOp (_, e) -> 1 + size e
    | EBinOp (e1, _, e2) -> 1 + size e1 + size e2
    | ELet (_, edef, ebody) -> 1 + 1 + 1 + size edef + size ebody
    | EIf (econd, ethen, eelse) -> 1 + size econd + size ethen + size eelse
    | EFix (_, _, ebody) | EFun (_, _, ebody) -> 1 + 1 + 1 + size ebody
    | EPair (e1, e2) -> 1 + size e1 + size e2

  let rec from_val (v : value) : t =
    match v with
    | VInt n -> EInt n
    | VBool b -> EBool b
    | VFun (x, typ, e) -> EFun (x, typ, e)
    | VPair (e1, e2) -> EPair (from_val e1, from_val e2)
    | VNil -> ENil
    | _ -> raise (Failure "Cannot be changed to expr")

  (* Each edge is represented as (index of start node, index of end node, edge type) *)
  type edge = int * int * int
  type node = int
  type graph = node list * edge list

  (* A list of variables and the node index of their declaration in the code *)
  type varlist = (Var.t * int) list

  let rec from_list (nodes : node list) (edges : edge list) (root : int) : t =
    let get_adj_nodes (edges : edge list) (start_node : int) : edge list =
      List.filter (fun (start, _, _) -> start = start_node) edges
    in
    let get_nth_child (edges : edge list) (n : int) : int =
      let child = List.filter (fun (_, _, idx) -> idx = n) edges in
      match child with
      | [] -> raise (TranslationError "No child when there should be")
      | [ (_, stop, _) ] -> stop
      | _ -> raise (TranslationError "More than one child at this position")
    in
    let tag = List.nth nodes root in
    let new_node = tag_to_node tag in
    match new_node with
    | EInt n -> EInt n
    | EBool b -> EBool b
    | EVar x -> EVar x
    | EUnOp (op, _) ->
        let adj_nodes = get_adj_nodes edges root in
        EUnOp (op, from_list nodes edges (get_nth_child adj_nodes 1))
    | EBinOp (_, op, _) ->
        let adj_nodes = get_adj_nodes edges root in
        EBinOp
          ( from_list nodes edges (get_nth_child adj_nodes 1),
            op,
            from_list nodes edges (get_nth_child adj_nodes 2) )
    | ELet (_, _, _) ->
        let adj_nodes = get_adj_nodes edges root in
        let varname =
          match from_list nodes edges (get_nth_child adj_nodes 1) with
          | EVar x -> x
          | _ -> raise (SyntaxError "Expression in variable name")
        in
        ELet
          ( varname,
            from_list nodes edges (get_nth_child adj_nodes 2),
            from_list nodes edges (get_nth_child adj_nodes 3) )
    | EIf (_, _, _) ->
        let adj_nodes = get_adj_nodes edges root in
        EIf
          ( from_list nodes edges (get_nth_child adj_nodes 1),
            from_list nodes edges (get_nth_child adj_nodes 2),
            from_list nodes edges (get_nth_child adj_nodes 3) )
    | EFun (_, _, _) ->
        let adj_nodes = get_adj_nodes edges root in
        let varname =
          match from_list nodes edges (get_nth_child adj_nodes 1) with
          | EVar x -> x
          | _ -> raise (SyntaxError "Expression in variable name")
        in
        EFun
          ( varname,
            Typ.from_list nodes edges (get_nth_child adj_nodes 2),
            from_list nodes edges (get_nth_child adj_nodes 3) )
    | EFix (_, _, _) ->
        let adj_nodes = get_adj_nodes edges root in
        let varname =
          match from_list nodes edges (get_nth_child adj_nodes 1) with
          | EVar x -> x
          | _ -> raise (SyntaxError "Expression in variable name")
        in
        EFix
          ( varname,
            Typ.from_list nodes edges (get_nth_child adj_nodes 2),
            from_list nodes edges (get_nth_child adj_nodes 3) )
    | EPair (_, _) ->
        let adj_nodes = get_adj_nodes edges root in
        EPair
          ( from_list nodes edges (get_nth_child adj_nodes 1),
            from_list nodes edges (get_nth_child adj_nodes 2) )
    | EHole -> EHole
    | ENil -> ENil

  (* let rec to_list (e : z_t) : graph * CursorInfo =
     let add_node (nodes : node list) (tag : int) : node list * int =
       let new_nodes = nodes @ [ tag ] in
       (new_nodes, List.length nodes)
     in
     let add_edge (edges : edge list) (new_edge : edge) : edge list =
       new_edge :: edges
     in
     let add_var (var : Var.t) (index : int) (vars : varlist) : varlist =
       (var, index) :: vars
     in
     let find_var (target : string) (vars : varlist) : int =
       let indices =
         List.filter (fun (var, index) -> Var.equal target var) vars
       in
       match indices with
       | (_, index) :: tl -> index
       | [] -> raise (SyntaxError "Expression not closed")
     in
     let append_type_tree (nodes : node list) (edges : edge list)
         (ty_nodes : node list) (ty_edges : edge list) (root : int) : graph * int
         =
       let len = List.length nodes in
       let ty_edges =
         List.map (fun (x, y, z) -> (x + len, y + len, z)) ty_edges
       in
       ((nodes @ ty_nodes, edges @ ty_edges), root + len)
     in
     let rec to_list_aux (e : t) (nodes : node list) (edges : edge list)
         (vars : varlist) : graph * int * varlist =
       let add_subtree (e : t) (nodes : node list) (edges : edge list)
           (root : int) (num_child : int) : graph =
         let (nodes, edges), new_root, _ = to_list_aux e nodes edges vars in
         let edges = add_edge edges (root, new_root, num_child) in
         (nodes, edges)
       in
       let tag = node_to_tag e in
       let nodes, root = add_node nodes tag in
       match e with
       | EInt _ | EBool _ | EHole | ENil -> ((nodes, edges), root, vars)
       | EVar x ->
           let edges = add_edge edges (find_var x vars, root, -1) in
           ((nodes, edges), root, vars)
       | EUnOp (_, e) -> (add_subtree e nodes edges root 1, root, vars)
       | EBinOp (e1, _, e2) | EPair (e1, e2) ->
           let nodes, edges = add_subtree e1 nodes edges root 1 in
           (add_subtree e2 nodes edges root 2, root, vars)
       | EFun (x, ty, e) | EFix (x, ty, e) ->
           let nodes, new_root = add_node nodes (node_to_tag (EVar x)) in
           let edges = add_edge edges (root, new_root, 1) in
           let vars = add_var x new_root vars in
           let (ty_nodes, ty_edges), new_root = Typ.to_list ty in
           let (nodes, edges), new_root =
             append_type_tree nodes edges ty_nodes ty_edges new_root
           in
           let edges = add_edge edges (root, new_root, 2) in
           (add_subtree e nodes edges root 3, root, vars)
       | ELet (x, edef, ebody) ->
           let nodes, new_root = add_node nodes (node_to_tag (EVar x)) in
           let edges = add_edge edges (root, new_root, 1) in
           let nodes, edges = add_subtree edef nodes edges root 2 in
           let vars = add_var x new_root vars in
           (add_subtree ebody nodes edges root 3, root, vars)
       | EIf (econd, ethen, eelse) ->
           let nodes, edges = add_subtree econd nodes edges root 1 in
           let nodes, edges = add_subtree ethen nodes edges root 2 in
           (add_subtree eelse nodes edges root 3, root, vars)
     in
     let graph, _, _ = to_list_aux (unzip_ast e) [] [] in
     (graph, get_cursor_info e) *)

  (* Convert an unzipped ast into a zipped one, by selecting the root *)
  let select_root (e : t) : z_t = Cursor e

  (* let rec unzip_ast (tree : z_t) : t =
     match tree with
     | Cursor arg -> arg
     | EUnOp_L (unop, l_child) -> EUnOp (unop, unzip_ast l_child)
     | EBinOp_L (l_child, binop, r_child) ->
         EBinOp (unzip_ast l_child, binop, r_child)
     | EBinOp_R (l_child, binop, r_child) ->
         EBinOp (l_child, binop, unzip_ast r_child)
     | ELet_L (var, l_child, r_child) -> ELet (var, unzip_ast l_child, r_child)
     | ELet_R (var, l_child, r_child) -> ELet (var, l_child, unzip_ast r_child)
     | EIf_L (l, c, r) -> EIf (unzip_ast l, c, r)
     | EIf_C (l, c, r) -> EIf (l, unzip_ast c, r)
     | EIf_R (l, c, r) -> EIf (l, c, unzip_ast r)
     | EPair_L (l, r) -> EPair (unzip_ast l, r)
     | EPair_R (l, r) -> EPair (l, unzip_ast r)
     | EFun_L (var_n, var_t, child) -> EFun (var_n, var_t, unzip_ast child)
     | EFix_L (var_n, var_t, child) -> EFix (var_n, var_t, unzip_ast child) *)
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
    | Fun of Var.t * Typ.t
    | Fix of Var.t * Typ.t
    | Pair_L
    | Pair_R

  type dir = Parent | Child of int

  (* write a numbered action to inser all of <- *)
  (* Have some sort of default value analog for type t *)
  (* Look at only allowing inserts on empty holes... *)
  (* maybe have delete move the subtree into 'copy' register *)

  type t =
    (*| Del                     (* Action Number: 0 *)
      (* | Finish                  Action Number: 1 *)*)
    | Move of dir (* Action Number: 2-5 *)
    | Construct of shape
  (* Action Number: 6- (36 ish) *)

  type tag = int

  type cursorInfo = {
    current_term : Expr.t;
    (*the currently focussed term (use to decide whether we can go down) *)
    parent_term : Expr.t option;
    (* parent of current term (use to decide whether we can go up)  *)
    ctx : (Var.t * int) list;
    (*mapping of vars in scope to types (use to determine vars in scope)    *)
    expected_ty : Typ.t option;
    (* analyzed type of cursor_term; build up through recursion (use with ctx to determine viable insert actions) *)
    actual_ty : Typ.t;
        (* result of calling Syn on current_term (use to determine wrapping viability)  *)
  }
  (*  Contains short-form avaliable actions*)
  (* In the format (Parent avaliable,
                   max child number (if 0 no children exist),
                   can_construct?
                   A list of 10 bools indicating if variables 'v0' ... 'v9' have been seen )*)

  let tag_to_action (action : tag) =
    let _ = action in
    Move Parent
end
