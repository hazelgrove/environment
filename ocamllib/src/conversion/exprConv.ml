(* Converts expressions to ints/lists *)
exception SyntaxError of string
exception TranslationError of string

open Expr

(* Convert AST represented by OCaml sum type to string *)
let rec to_string (e : p_t) : string =
  match e with
  | Var x -> Var.to_string x ^ " "
  | Const c -> (
      match c with
      | Int n -> string_of_int n ^ " "
      | Bool b -> string_of_bool b ^ " "
      | Nil -> "[] ")
  | UnOp (_, e) -> "(-" ^ to_string e ^ ") "
  | BinOp (e1, op, e2) ->
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
        | OpAnd -> "&&"
        | OpOr -> "||"
      in
      "(" ^ to_string e1 ^ " " ^ op_string ^ " " ^ to_string e2 ^ ") "
  | If (cond, e1, e2) ->
      "(if " ^ to_string cond ^ " then " ^ to_string e1 ^ " else "
      ^ to_string e2 ^ ") "
  | Let (x, Fix (_, _, e1), e2) ->
      "let rec " ^ Var.to_string x ^ resolve_fun e1 ^ " in " ^ to_string e2
      ^ " "
  | Let (x, Fun (arg, ty, e1), e2) ->
      "let " ^ Var.to_string x
      ^ resolve_fun (Fun (arg, ty, e1))
      ^ " in " ^ to_string e2 ^ " "
  | Let (x, e1, e2) ->
      "let " ^ Var.to_string x ^ " = " ^ to_string e1 ^ " in " ^ to_string e2
      ^ " "
  | Fix (_, _, _) -> raise (SyntaxError "Incorrect syntax with fix")
  | Fun (x, ty, e) ->
      if ty = Type.Hole
      then "(fun " ^ Var.to_string x ^ " -> " ^ to_string e ^ ") "
      else
        "(fun (" ^ Var.to_string x ^ " : " ^ TypeConv.to_string ty ^ ") -> "
        ^ to_string e ^ ") "
  | Pair (e1, e2) -> "(" ^ to_string e1 ^ ", " ^ to_string e2 ^ ") "
  | Hole -> "? "
  | Map (e1, e2) -> "Map ( " ^ to_string e1 ^ ", " ^ to_string e2 ^ ") "
  | Filter (e1, e2) -> "Filter ( " ^ to_string e1 ^ ", " ^ to_string e2 ^ ") "
  | ListEq (e1, e2) ->
      "List.equal ( " ^ to_string e1 ^ ", " ^ to_string e2 ^ ") "
  | Match (e, (p1, e1), (p2, e2)) ->
      "(match " ^ to_string e ^ " with | " ^ PatternConv.to_string p1 ^ " -> "
      ^ to_string e1 ^ " | " ^ PatternConv.to_string p2 ^ " -> " ^ to_string e2
      ^ ") "
  | Assert e -> "assert(" ^ to_string e ^ ") "
  | Fold (func, acc, list) -> "Fold ( " ^ to_string func ^ ", " ^ to_string acc ^ ", " ^ to_string list ^ ")"

and resolve_fun (e : p_t) : string =
  match e with
  | Fun (x, ty, e) ->
      if ty = Type.Hole
      then " " ^ Var.to_string x ^ resolve_fun e
      else
        " (" ^ Var.to_string x ^ " : " ^ TypeConv.to_string ty ^ ") "
        ^ resolve_fun e
  | _ -> " = " ^ to_string e ^ " "

let node_list =
  [
    EUnOp (OpNeg, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpPlus, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpMinus, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpTimes, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpDiv, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpLt, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpLe, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpGt, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpGe, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpEq, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpNe, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpCons, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpAp, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpAnd, make_dummy_node EHole);
    EBinOp (make_dummy_node EHole, OpOr, make_dummy_node EHole);
    EIf (make_dummy_node EHole, make_dummy_node EHole, make_dummy_node EHole);
    ELet (Var.undef_var, make_dummy_node EHole, make_dummy_node EHole);
    EFun (Var.undef_var, Type.make_dummy_node THole, make_dummy_node EHole);
    EFix (Var.undef_var, Type.make_dummy_node THole, make_dummy_node EHole);
    EPair (make_dummy_node EHole, make_dummy_node EHole);
    EMap (make_dummy_node EHole, make_dummy_node EHole);
    EFilter (make_dummy_node EHole, make_dummy_node EHole);
    EFold (make_dummy_node EHole, make_dummy_node EHole, make_dummy_node EHole);
    EListEq (make_dummy_node EHole, make_dummy_node EHole);
    EAssert (make_dummy_node EHole);
    EMatch
      ( make_dummy_node EHole,
        (Pattern.make_dummy_node PWild, make_dummy_node EHole),
        (Pattern.make_dummy_node PWild, make_dummy_node EHole) );
    EHole;
    EConst Nil;
    EConst (Bool true);
    EConst (Bool false);
    EConst (Int (-2));
    EConst (Int (-1));
    EConst (Int 0);
    EConst (Int 1);
    EConst (Int 2);
    EConst (Int 3);
    EConst (Int 4);
    EConst (Int 5);
  ]
  @ List.init Var.max_num_vars (fun i -> EVar i)

let num_nodes = List.length node_list

let node_list_equal (e1 : node) (e2 : node) : bool =
  if e1 = e2
  then true
  else
    match (e1, e2) with
    | EUnOp (op1, _), EUnOp (op2, _) -> Expr.unop_equal op1 op2
    | EBinOp (_, op1, _), EBinOp (_, op2, _) -> Expr.binop_equal op1 op2
    | EIf _, EIf _
    | ELet _, ELet _
    | EFun _, EFun _
    | EFix _, EFix _
    | EPair _, EPair _
    | EAssert _, EAssert _
    | EMap _, EMap _
    | EFilter _, EFilter _
    | EFold _, EFold _ 
    | EListEq _, EListEq _
    | EMatch _, EMatch _ ->
        true
    | _ -> false

(* Convert each unique AST node to an integer *)
let node_to_tag (e : t) : int =
  let rec find_node x lst c =
    match lst with
    | [] -> raise (Failure ("Invalid node " ^ (e |> Expr.strip |> to_string)))
    | hd :: tl -> if node_list_equal x hd then c else find_node x tl (c + 1)
  in
  find_node e.node node_list (PatternConv.num_nodes + TypeConv.num_nodes)

(* Convert each integer to a type of node with expression holes padded for its children *)
let tag_to_node (tag : int) : t =
  let tag = tag - TypeConv.num_nodes - PatternConv.num_nodes in
  let node =
    try List.nth node_list tag
    with Failure _ | Invalid_argument _ ->
      raise (Failure ("Invalid node index " ^ string_of_int tag))
  in
  make_node node

(* Shorthands for following functions *)
type edge = int * int * int
type graph = int list * edge list
type varlist = (Var.t * int) list

let rec from_list ~(nodes : int list) ~(edges : edge list) ~(root : int) : t =
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
  let node = tag_to_node tag in
  let new_node =
    match node.node with
    | EConst c -> EConst c
    | EVar x -> EVar x
    | EUnOp (op, _) ->
        let adj_nodes = get_adj_nodes edges root in
        EUnOp (op, from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0))
    | EBinOp (_, op, _) ->
        let adj_nodes = get_adj_nodes edges root in
        EBinOp
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0),
            op,
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1) )
    | EMap (_, _) ->
        let adj_nodes = get_adj_nodes edges root in
        EMap
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1) )
    | EFilter (_, _) ->
        let adj_nodes = get_adj_nodes edges root in
        EFilter
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1) )
    | EListEq (_, _) ->
        let adj_nodes = get_adj_nodes edges root in
        EListEq
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1) )
    | ELet (_, _, _) ->
        let adj_nodes = get_adj_nodes edges root in
        let varname =
          match
            (from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0)).node
          with
          | EVar x -> x
          | _ -> raise (SyntaxError "Expression in variable name")
        in
        ELet
          ( varname,
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 2) )
    | EIf _ ->
        let adj_nodes = get_adj_nodes edges root in
        EIf
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 2) )
    | EFold _ ->
        let adj_nodes = get_adj_nodes edges root in
        EFold
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 2) )
    | EFun _ ->
        let adj_nodes = get_adj_nodes edges root in
        let varname =
          match
            (from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0)).node
          with
          | EVar x -> x
          | _ -> raise (SyntaxError "Expression in variable name")
        in
        EFun
          ( varname,
            TypeConv.from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 2) )
    | EFix _ ->
        let adj_nodes = get_adj_nodes edges root in
        let varname =
          match
            (from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0)).node
          with
          | EVar x -> x
          | _ -> raise (SyntaxError "Expression in variable name")
        in
        EFix
          ( varname,
            TypeConv.from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 2) )
    | EPair _ ->
        let adj_nodes = get_adj_nodes edges root in
        EPair
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0),
            from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 1) )
    | EAssert _ ->
        let adj_nodes = get_adj_nodes edges root in
        EAssert (from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0))
    | EMatch _ ->
        let adj_nodes = get_adj_nodes edges root in
        EMatch
          ( from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 0),
            ( PatternConv.from_list ~nodes ~edges
                ~root:(get_nth_child adj_nodes 1),
              from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 2) ),
            ( PatternConv.from_list ~nodes ~edges
                ~root:(get_nth_child adj_nodes 3),
              from_list ~nodes ~edges ~root:(get_nth_child adj_nodes 4) ) )
    | EHole -> EHole
  in
  { node with node = new_node }

(* let%test_module "Test Expr.from_list" =
   (module struct
     let%test _ = from_list ~nodes:[ 35 ] ~edges:[] ~root:0 = EInt 0

     let%test _ =
       from_list
         ~nodes:[ 15; 38; 25; 1; 3; 37; 38; 36 ]
         ~edges:
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
         ~root:0
       = EFun
           ( "x",
             THole,
             EBinOp (EBinOp (EInt 2, OpTimes, EVar "x"), OpPlus, EInt 1) )
   end) *)

let to_list (e : z_t) : graph =
  let add_node (nodes : int list) (tag : int) : int list * int =
    let new_nodes = nodes @ [ tag ] in
    (new_nodes, List.length nodes)
  in
  let add_edge (edges : edge list) (new_edge : edge) : edge list =
    new_edge :: edges
  in
  let add_var (var : Var.t) (index : int) (vars : varlist) : varlist =
    (var, index) :: vars
  in
  let find_var (target : Var.t) (vars : varlist) : int =
    let indices = List.filter (fun (var, index) -> Var.equal target var) vars in
    match indices with
    | (_, index) :: tl -> index
    | [] -> raise (SyntaxError "Expression not closed")
  in
  let append_type_tree (nodes : int list) (edges : edge list)
      (ty_nodes : int list) (ty_edges : edge list) (root : int) : graph * int =
    let len = List.length nodes in
    let ty_edges = List.map (fun (x, y, z) -> (x + len, y + len, z)) ty_edges in
    ((nodes @ ty_nodes, edges @ ty_edges), root + len)
  in
  let append_pattern_tree (nodes : int list) (edges : edge list)
      (vars : varlist) (p_nodes : int list) (p_edges : edge list) (root : int)
      (p_vars : varlist) : graph * int * varlist =
    let len = List.length nodes in
    let p_edges = List.map (fun (x, y, z) -> (x + len, y + len, z)) p_edges in
    let p_vars = List.map (fun (var, index) -> (var, index + len)) p_vars in
    ((nodes @ p_nodes, edges @ p_edges), root + len, vars @ p_vars)
  in
  let rec to_list_aux (e : t) (nodes : int list) (edges : edge list)
      (vars : varlist) : graph * int * varlist =
    let add_subtree (e : t) (nodes : int list) (edges : edge list)
        (vars : varlist) (root : int) (num_child : int) : graph =
      let (nodes, edges), new_root, _ = to_list_aux e nodes edges vars in
      let edges = add_edge edges (root, new_root, num_child) in
      (nodes, edges)
    in
    let tag = node_to_tag e in
    let nodes, root = add_node nodes tag in
    match e.node with
    | EConst _ | EHole -> ((nodes, edges), root, vars)
    | EVar x ->
        let edges = add_edge edges (find_var x vars, root, -1) in
        ((nodes, edges), root, vars)
    | EUnOp (_, e) -> (add_subtree e nodes edges vars root 0, root, vars)
    | EAssert e -> (add_subtree e nodes edges vars root 0, root, vars)
    | EBinOp (e1, _, e2)
    | EPair (e1, e2)
    | EMap (e1, e2)
    | EFilter (e1, e2)
    | EListEq (e1, e2) ->
        let nodes, edges = add_subtree e1 nodes edges vars root 0 in
        (add_subtree e2 nodes edges vars root 1, root, vars)
    | EFun (x, ty, e) | EFix (x, ty, e) ->
        let nodes, new_root =
          add_node nodes (node_to_tag (make_dummy_node (EVar x)))
        in
        let edges = add_edge edges (root, new_root, 0) in
        let vars = add_var x new_root vars in
        let (ty_nodes, ty_edges), new_root = TypeConv.to_list ty in
        let (nodes, edges), new_root =
          append_type_tree nodes edges ty_nodes ty_edges new_root
        in
        let edges = add_edge edges (root, new_root, 1) in
        (add_subtree e nodes edges vars root 2, root, vars)
    | ELet (x, edef, ebody) ->
        let nodes, new_root =
          add_node nodes (node_to_tag (make_dummy_node (EVar x)))
        in
        let edges = add_edge edges (root, new_root, 0) in
        let nodes, edges = add_subtree edef nodes edges vars root 1 in
        let vars = add_var x new_root vars in
        (add_subtree ebody nodes edges vars root 2, root, vars)
    | EFold (e1, e2, e3) 
    | EIf (e1, e2, e3) ->
        let nodes, edges = add_subtree e1 nodes edges vars root 0 in
        let nodes, edges = add_subtree e2 nodes edges vars root 1 in
        (add_subtree e3 nodes edges vars root 2, root, vars)
    | EMatch (escrut, (p1, e1), (p2, e2)) ->
        let nodes, edges = add_subtree escrut nodes edges vars root 0 in
        let (p_nodes, p_edges), new_root, p_vars = PatternConv.to_list p1 in
        let (nodes, edges), new_root, vars =
          append_pattern_tree nodes edges vars p_nodes p_edges new_root p_vars
        in
        let edges = add_edge edges (root, new_root, 1) in
        let nodes, edges = add_subtree e1 nodes edges vars root 2 in
        let (p_nodes, p_edges), new_root, p_vars = PatternConv.to_list p2 in
        let (nodes, edges), new_root, vars =
          append_pattern_tree nodes edges vars p_nodes p_edges new_root p_vars
        in
        let edges = add_edge edges (root, new_root, 3) in
        (add_subtree e2 nodes edges vars root 4, root, vars)
  in
  let graph, _, _ =
    try to_list_aux (unzip e) [] [] []
    with SyntaxError err ->
      raise (SyntaxError (err ^ "\n" ^ (e |> unzip |> strip |> to_string)))
  in
  graph

(* let%test_module "Test to_list" =
   (module struct
     let check_id e =
       let (nodes, edges), _ = to_list (Cursor e) in
       let changed_tree = from_list ~nodes ~edges ~root:0 in
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

let rec get_starter_list (e : Expr.t) : bool list =
  match e.node with
  | EVar _ | EConst _ | EHole -> [ e.starter ]
  | EUnOp (_, arg) | EAssert arg -> e.starter :: get_starter_list arg
  | EBinOp (arg1, _, arg2)
  | EPair (arg1, arg2)
  | EMap (arg1, arg2)
  | EFilter (arg1, arg2)
  | EListEq (arg1, arg2) ->
      e.starter :: (get_starter_list arg1 @ get_starter_list arg2)
  | EFun (x, ty, body) | EFix (x, ty, body) ->
      [ e.starter; e.starter ]
      @ TypeConv.get_starter_list ty
      @ get_starter_list body
  | ELet (x, edef, ebody) ->
      [ e.starter; e.starter ] @ get_starter_list edef @ get_starter_list ebody
  | EFold (e1, e2, e3)
  | EIf (e1, e2, e3) ->
      (e.starter :: get_starter_list e1)
      @ get_starter_list e2 @ get_starter_list e3
  | EMatch (escrut, (p1, e1), (p2, e2)) ->
      (e.starter :: get_starter_list escrut)
      @ PatternConv.get_starter_list p1
      @ get_starter_list e1
      @ PatternConv.get_starter_list p2
      @ get_starter_list e2
