(* Converting between OCaml sum types and ints/arrays for C *)
open Ast

(* Convert AST represented by OCaml sum type to string *)
let rec ast_to_string (e : Expr.t) : string =
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
        if ty = Typ.THole
        then "(fun " ^ x ^ " -> " ^ to_string e ^ ") "
        else
            "(fun (" ^ x ^ " : " ^ Typ.to_string ty ^ ") -> " ^ to_string e ^ ") "
    | EPair (e1, e2) -> "(" ^ to_string e1 ^ ", " ^ to_string e2 ^ ") "
    | EHole -> "<HOLE> "
    | ENil -> "[] "
and resolve_fun (e : Expr.t) : string =
    match e with
    | EFun (x, ty, e) ->
        if ty = Typ.THole
        then " " ^ x ^ resolve_fun e
        else " (" ^ x ^ " : " ^ Typ.to_string ty ^ ") " ^ resolve_fun e
    | _ -> " = " ^ to_string e ^ " "

(* Convert each unique AST node to an integer *)
let node_to_tag (node : Expr.t) : int =
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
    | node -> raise (Failure ((ast_to_string node) ^ "not supported yet"))

(* Convert each integer to a type of node with expression holes padded for its children *)
let tag_to_node (tag : int) : Expr.t =
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
