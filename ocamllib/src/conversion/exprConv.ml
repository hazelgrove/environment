(* Converts expressions to ints/lists *)
exception SyntaxError of string
exception TranslationError of string

open Expr

(* Convert AST represented by OCaml sum type to string *)
let rec to_string (e : t) : string =
  match e with
  | EVar x -> "<VAR>" ^ x ^ " "
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
        | OpCons -> "::"
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
      if ty = Type.THole
      then "(fun " ^ x ^ " -> " ^ to_string e ^ ") "
      else
        "(fun (" ^ x ^ " : " ^ TypeConv.to_string ty ^ ") -> " ^ to_string e
        ^ ") "
  | EPair (e1, e2) -> "(" ^ to_string e1 ^ ", " ^ to_string e2 ^ ") "
  | EHole -> "<HOLE> "
  | ENil -> "[] "

and resolve_fun (e : t) : string =
  match e with
  | EFun (x, ty, e) ->
      if ty = Type.THole
      then " " ^ x ^ resolve_fun e
      else " (" ^ x ^ " : " ^ TypeConv.to_string ty ^ ") " ^ resolve_fun e
  | _ -> " = " ^ to_string e ^ " "

(* Convert each unique AST node to an integer *)
let node_to_tag (node : t) : int =
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
      | OpCons -> 11
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
  | EVar "" -> 42
  | node -> raise (Failure (to_string node ^ "not supported yet"))

(* Convert each integer to a type of node with expression holes padded for its children *)
let tag_to_node (tag : int) : t =
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
  | 11 -> EBinOp (EHole, OpCons, EHole)
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
  | 42 -> EVar ""
  | _ -> raise (Failure "Node index not supported")

(* Shorthands for following functions *)
type edge = int * int * int
type node = int
type graph = node list * edge list
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
          TypeConv.from_list nodes edges (get_nth_child adj_nodes 2),
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
          TypeConv.from_list nodes edges (get_nth_child adj_nodes 2),
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
        [ 15; 38; 25; 1; 3; 37; 38; 36 ]
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

let to_list (e : z_t) : graph * CursorInfo.t =
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
    let indices = List.filter (fun (var, index) -> Var.equal target var) vars in
    match indices with
    | (_, index) :: tl -> index
    | [] -> raise (SyntaxError "Expression not closed")
  in
  let append_type_tree (nodes : node list) (edges : edge list)
      (ty_nodes : node list) (ty_edges : edge list) (root : int) : graph * int =
    let len = List.length nodes in
    let ty_edges = List.map (fun (x, y, z) -> (x + len, y + len, z)) ty_edges in
    ((nodes @ ty_nodes, edges @ ty_edges), root + len)
  in
  let rec to_list_aux (e : t) (nodes : node list) (edges : edge list)
      (vars : varlist) : graph * int * varlist =
    let add_subtree (e : t) (nodes : node list) (edges : edge list)
        (vars : varlist) (root : int) (num_child : int) : graph =
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
    | EUnOp (_, e) -> (add_subtree e nodes edges vars root 1, root, vars)
    | EBinOp (e1, _, e2) | EPair (e1, e2) ->
        let nodes, edges = add_subtree e1 nodes edges vars root 1 in
        (add_subtree e2 nodes edges vars root 2, root, vars)
    | EFun (x, ty, e) | EFix (x, ty, e) ->
        let nodes, new_root = add_node nodes (node_to_tag (EVar x)) in
        let edges = add_edge edges (root, new_root, 1) in
        let vars = add_var x new_root vars in
        let (ty_nodes, ty_edges), new_root = TypeConv.to_list ty in
        let (nodes, edges), new_root =
          append_type_tree nodes edges ty_nodes ty_edges new_root
        in
        let edges = add_edge edges (root, new_root, 2) in
        (add_subtree e nodes edges vars root 3, root, vars)
    | ELet (x, edef, ebody) ->
        let nodes, new_root = add_node nodes (node_to_tag (EVar x)) in
        let edges = add_edge edges (root, new_root, 1) in
        let nodes, edges = add_subtree edef nodes edges vars root 2 in
        let vars = add_var x new_root vars in
        (add_subtree ebody nodes edges vars root 3, root, vars)
    | EIf (econd, ethen, eelse) ->
        let nodes, edges = add_subtree econd nodes edges vars root 1 in
        let nodes, edges = add_subtree ethen nodes edges vars root 2 in
        (add_subtree eelse nodes edges vars root 3, root, vars)
  in
  let graph, _, _ = to_list_aux (unzip_ast e) [] [] [] in
  (graph, CursorInfo.get_cursor_info (Syntax.ZENode e))

let%test_module "Test to_list" =
  (module struct
    let check_id e =
      let (nodes, edges), _ = to_list (Cursor e) in
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
  end)
