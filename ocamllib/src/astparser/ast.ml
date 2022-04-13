open Type
open Sexplib.Std
open Var 


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
    | EFun_R of Var.t * Typ.t * z_t
    | EFun_L of Var.t * Typ.z_t * t (* TOOD: need to fix all our rerucsion operations now *) 
    | EFix_R of Var.t * Typ.t * z_t
    | EFix_L of Var.t * Typ.z_t * t (* TOOD: need to fix all our rerucsion operations now *) 
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
    | EBinOp (e1, _, e2) | EPair (e1, e2) -> 1 + size e1 + size e2
    | ELet (_, edef, ebody) -> 1 + 1 + size edef + size ebody
    | EIf (econd, ethen, eelse) -> 1 + size econd + size ethen + size eelse
    | EFix (_, ty, ebody) | EFun (_, ty, ebody) ->
        1 + 1 + Typ.size ty + size ebody

  let%test_module "Test Expr.size" =
    (module struct
      let%test _ = size (EInt 10) = 1
      let%test _ = size (EUnOp (OpNeg, EBinOp (EHole, OpPlus, EVar "x"))) = 4

      let%test _ =
        size
          (ELet
             ( "x",
               EIf (EBool true, EInt 3, EInt 4),
               EFun ("f", TProd (TInt, TInt), EBinOp (EVar "f", OpAp, EVar "x"))
             ))
        = 14
    end)

  let rec from_val (v : value) : t =
    match v with
    | VInt n -> EInt n
    | VBool b -> EBool b
    | VFun (x, typ, e) -> EFun (x, typ, e)
    | VPair (e1, e2) -> EPair (from_val e1, from_val e2)
    | VNil -> ENil
    | _ -> raise (Failure "Cannot be changed to expr")

  (* Convert an unzipped ast into a zipped one, by selecting the root *)
  let select_root (e : t) : z_t = Cursor e
  type edge = int * int * int
  type node = int
  type graph = node list * edge list

  let rec unzip_ast (tree : z_t) : t =
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
    | EFun_R (var_n, var_t, child) -> EFun (var_n, var_t, unzip_ast child)
    | EFun_L (var_n, var_t, child) -> EFun (var_n, Typ.unzip var_t, child ) (*unzip child type*)
    | EFix_R (var_n, var_t, child) -> EFix (var_n, var_t, unzip_ast child)
    | EFix_L (var_n, var_t, child) -> EFix (var_n, Typ.unzip var_t, child) (*unzip child type*)
  (* Each edge is represented as (index of start node, index of end node, edge type) *)
  let%test_module "Test Expr.unzip_ast" =
    (module struct
      let%test _ = unzip_ast (Cursor(EHole)) = EHole
      let%test _ = unzip_ast (EPair_L(Cursor(EInt 7),EBool false)) = EPair(EInt 7,EBool false)
    end)
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

  let%test_module "Test Expr.from_list" =
    (module struct
      let%test _ = from_list [ 35 ] [] 0 = EInt 0

      let%test _ =
        from_list
          [ 15; 38; 24; 1; 3; 37; 38; 36 ]
          [
            (0, 1, 1);
            (0, 2, 2);
            (0, 3, 3);
            (3, 4, 1);
            (3, 7, 2);
            (4, 5, 1);
            (4, 6, 2);
            (6, 1, -1);
          ]
          0
        = EFun
            ( "x",
              THole,
              EBinOp (EBinOp (EInt 2, OpTimes, EVar "x"), OpPlus, EInt 1) )
    end)

  (* let%test_module "Test Expr.to_list" =
    (module struct
      let check_id e =
        let (nodes, edges), _ = to_list (select_root e) in
        let changed_tree = from_list nodes edges 0 in
        e = changed_tree
      let%test _ =
        check_id
          (EFun
             ( "x",
               THole,
               EBinOp (EBinOp (EInt 2, OpTimes, EVar "x"), OpPlus, EInt 1) ))

      let%test _ =
        check_id
          (ELet
             ( "x",
               EFix
                 ( "x",
                   THole,
                   EFun
                     ( "y",
                       TInt,
                       EIf
                         ( EBinOp (EVar "y", OpLt, EInt 1),
                           EInt 1,
                           EBinOp
                             ( EVar "y",
                               OpTimes,
                               EBinOp
                                 ( EVar "x",
                                   OpAp,
                                   EBinOp (EVar "y", OpMinus, EInt 1) ) ) ) ) ),
               EBinOp (EVar "x", OpAp, EInt 2) ))
    end) *)

  (* Change tree representation to string to better interpret graph *)
  let rec to_string (e : t) : string =
    match e with
    | EVar x -> x ^ " "
    | EInt n -> string_of_int n ^ " "
    | EBool b -> string_of_bool b ^ " "
    | EUnOp (_, e) -> "(-" ^ to_string e ^ ") "
    | EBinOp (e1, op, e2) ->
        let op_string =
          match op with
          | OpPlus -> "+"
          | OpMinus -> "-"
          | OpTimes -> "*"
          | OpDiv -> "/"
          | OpLt -> "<"
          | OpLe -> "<="
          | OpGt -> ">"
          | OpGe -> ">="
          | OpEq -> "="
          | OpNe -> "!="
          | OpCon -> "::"
          | OpAp -> " "
        in
        "(" ^ to_string e1 ^ " " ^ op_string ^ " " ^ to_string e2 ^ ") "
    | EIf (cond, e1, e2) ->
        "(if " ^ to_string cond ^ " then " ^ to_string e1 ^ " else "
        ^ to_string e2 ^ ") "
    | ELet (x, EFix (_, _, e1), EHole) -> "let rec " ^ x ^ resolve_fun e1 ^ " "
    | ELet (x, EFix (_, _, e1), e2) ->
        "let rec " ^ x ^ resolve_fun e1 ^ " in " ^ to_string e2 ^ " "
    | ELet (x, EFun (arg, ty, e1), EHole) ->
        "let " ^ x ^ resolve_fun (EFun (arg, ty, e1)) ^ " "
    | ELet (x, EFun (arg, ty, e1), e2) ->
        "let " ^ x
        ^ resolve_fun (EFun (arg, ty, e1))
        ^ " in " ^ to_string e2 ^ " "
    | ELet (x, e1, EHole) -> "let " ^ x ^ " = " ^ to_string e1 ^ " "
    | ELet (x, e1, e2) ->
        "let " ^ x ^ " = " ^ to_string e1 ^ " in " ^ to_string e2 ^ " "
    | EFix (_, _, _) -> raise (SyntaxError "Incorrect syntax with fix")
    | EFun (x, ty, e) ->
        if ty = Typ.THole
        then "(fun " ^ x ^ " -> " ^ to_string e ^ ") "
        else
          "(fun (" ^ x ^ " : " ^ Typ.to_string ty ^ ") -> " ^ to_string e ^ ") "
    | EPair (e1, e2) -> "(" ^ to_string e1 ^ ", " ^ to_string e2 ^ ") "
    | EHole -> "<HOLE> "
    | ENil -> "[] "

  and resolve_fun (e : t) : string =
    match e with
    | EFun (x, ty, e) ->
        if ty = Typ.THole
        then " " ^ x ^ resolve_fun e
        else " (" ^ x ^ " : " ^ Typ.to_string ty ^ ") " ^ resolve_fun e
    | _ -> " = " ^ to_string e ^ " "
  end


module CursorInfo = struct
  type t = {
    current_term : Expr.t;
    (*the currently focussed term (use to decide whether we can go down) *)
    (*is_root: bool; (*boolean value of whether or not cursor is at root. simpler version of vv*)  *)
    parent_term : Expr.t option;
    (* parent of current term (use to decide whether we can go up)  *)
    ctx : (Var.t * int) list;
    (*mapping of vars in scope to types (use to determine vars in scope)    *)
    expected_ty : Typ.t option;
    (* analyzed type of cursor_term; build up through recursion (use with ctx to determine viable insert actions) *)
    actual_ty : Typ.t;
        (* result of calling Syn on current_term (use to determine wrapping viability)  *)
  }
  [@@deriving sexp]

end


(* had issues refactoring this into a seperate file *)
module SyntaxTree = struct
  type t = (* mixed type- zippers and non-zippers*)
  | ENode of Expr.t 
  | TNode  of Typ.t
  [@@deriving sexp]

  type z_t  = 
  | ZENode of Expr.z_t 
  | ZTNode of Typ.z_t
  [@@deriving sexp]

  let rec size (tree:t) : int = 
    match tree with
    | ENode (EVar _  |EInt _ |EBool _ | EHole | ENil ) -> 1 
    | ENode EUnOp (_,arg) -> size (ENode arg)+1
    | ENode (EBinOp (argl, _,argr) 
            | ELet (_,argl, argr )
            | EPair (argl, argr))  -> size (ENode argl) + size (ENode argr) +1
    | ENode EIf (argl, argc, argr) -> size (ENode argl) + size (ENode argc) + size (ENode argr) +1
    | ENode (EFun (_,typ, arg) | EFix (_,typ, arg)) -> Typ.size typ + size (ENode arg) + 1 
    | TNode type_tree -> Typ.size type_tree
  
  let rec zsize (tree:z_t) : int = 
    match tree with 
    | ZENode (Cursor cursed) -> size (ENode cursed)
    | ZENode (EUnOp_L (_,argl) ) -> zsize (ZENode argl)+1
    | ZENode  EBinOp_L (argl, _, argr) -> zsize (ZENode argl) +  size (ENode argr) +1
    | ZENode  EBinOp_R (argl, _, argr) -> size (ENode argl) +  zsize (ZENode argr)+1
    | ZENode  ELet_L (_,argl,argr )    -> zsize (ZENode argl) + size (ENode argr) +1
    | ZENode  ELet_R (_,argl,argr )    -> size (ENode argl) + zsize (ZENode argr) +1
    | ZENode  EIf_L (argl, argc,argr) -> zsize (ZENode argl)+ size (ENode argc) + size (ENode argr)+1
    | ZENode  EIf_C (argl, argc,argr) -> size (ENode argl) + zsize (ZENode argc) + size (ENode argr)+1
    | ZENode  EIf_R (argl, argc,argr) -> size (ENode argl) + size (ENode argc) +  zsize (ZENode argr)+1
    | ZENode  EFun_L (_, typ, argr) -> size (ENode argr) + size (ENode argr) +1
    | ZENode  EFun_R (_, typ, argr) -> size (TNode typ) +  zsize (ZENode argr) +1
    | ZENode  EFix_L (_, typ, argr) -> zsize (ZTNode typ) + size (ENode argr) +1
    | ZENode  EFix_R (_, typ, argr) -> size (TNode typ) +  zsize (ZENode argr) +1
    | ZENode  EPair_L (argl, argr)  -> zsize (ZENode argl) +size (ENode argr)+1
    | ZENode  EPair_R (argl, argr)  -> size (ENode argl) + zsize (ZENode argr)+1
    | ZTNode type_tree -> Typ.size (Typ.unzip type_tree)
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
    
    | TypInt 
    | TypBool 
    | TypArrow_L 
    | TypArrow_R
    | TypList  (*beacause there's only one child no need for option*)
    | TypHole 


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

  (*  Contains short-form avaliable actions*)
  (* In the format (Parent avaliable,
                   max child number (if 0 no children exist),
                   can_construct?
                   A list of 10 bools indicating if variables 'v0' ... 'v9' have been seen )*)

  let tag_to_action (action : tag) =
    let _ = action in
    Move Parent
end
