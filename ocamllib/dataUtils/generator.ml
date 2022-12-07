(* Generate code n actions away from original *)
let rec generate (e : Expr.z_t) (n : int) : Expr.z_t =
  let num_child (e : Expr.t) : int =
    let e = Expr.strip e in
    match e with
    | Const _ | Hole | Var _ -> 0
    | BinOp (e1, _, e2)
    | Pair (e1, e2)
    | Let (_, e1, e2)
    | Map (e1, e2)
    | Filter (e1, e2)
    | ListEq (e1, e2) -> (
        match (e1, e2) with Hole, Hole -> 0 | Hole, _ | _, Hole -> 1 | _ -> 2)
    | If (e1, e2, e3) | Fold (e1, e2, e3) -> (
        match (e1, e2, e3) with
        | Hole, Hole, Hole -> 0
        | _, Hole, Hole | Hole, _, Hole | Hole, Hole, _ -> 1
        | Hole, _, _ | _, Hole, _ | _, _, Hole -> 2
        | _ -> 3)
    | Fun (_, _, e) | Fix (_, _, e) | UnOp (_, e) | Assert e -> (
        match e with Hole -> 0 | _ -> 1)
    | Match (e, (p1, e1), (p2, e2)) ->
        let expr_num =
          match (e, e1, e2) with
          | Hole, Hole, Hole -> 0
          | _, Hole, Hole | Hole, _, Hole | Hole, Hole, _ -> 1
          | Hole, _, _ | _, Hole, _ | _, _, Hole -> 2
          | _ -> 3
        in
        let pat_num =
          match (p1, p2) with
          | Wild, Wild -> 0
          | Wild, _ | _, Wild -> 1
          | _ -> 2
        in
        expr_num + pat_num
  in
  let num_child_pattern (p : Pattern.t) : int =
    let p = Pattern.strip p in
    match p with
    | Const _ | Var _ | Wild -> 0
    | Cons (p1, p2) -> (
        match (p1, p2) with Wild, Wild -> 0 | Wild, _ | _, Wild -> 1 | _ -> 2)
  in
  let check_child (e : Expr.t) (n : int) : bool =
    let e = Expr.strip e in
    match e with
    | Const _ | Hole | Var _ -> false (* Impossible to do unwrap on these *)
    | UnOp _ | Fun _ | Fix _ | Assert _ ->
        n = 1 (* No subtree to erase if we dont consider types *)
    | BinOp (e1, _, e2)
    | Pair (e1, e2)
    | Let (_, e1, e2)
    | Map (e1, e2)
    | Filter (e1, e2)
    | ListEq (e1, e2) -> (
        match n with
        | 0 -> ( match e2 with Hole -> true | _ -> false)
        | 1 -> ( match e1 with Hole -> true | _ -> false)
        | _ -> false)
    | If (e1, e2, e3) | Fold (e1, e2, e3) -> (
        match n with
        | 0 -> ( match (e2, e3) with Hole, Hole -> true | _ -> false)
        | 1 -> ( match (e1, e3) with Hole, Hole -> true | _ -> false)
        | 2 -> ( match (e1, e2) with Hole, Hole -> true | _ -> false)
        | _ -> false)
    | Match (e, (p1, e1), (p2, e2)) -> (
        match n with
        | 0 -> ( match (e1, e2) with Hole, Hole -> true | _ -> false)
        | 1 -> ( match (e, e2) with Hole, Hole -> true | _ -> false)
        | 2 -> ( match (e, e1) with Hole, Hole -> true | _ -> false)
        | _ -> false)
  in
  let check_child_pattern (p : Pattern.t) (n : int) : bool =
    let p = Pattern.strip p in
    match p with
    | Const _ | Var _ | Wild -> false (* Impossible to do unwrap on these *)
    | Cons (p1, p2) -> (
        match n with
        | 0 -> ( match p2 with Wild -> true | _ -> false)
        | 1 -> ( match p1 with Wild -> true | _ -> false)
        | _ -> false)
  in
  if n = 0
  then e
  else
    let cursor_info = CursorInfo.get_cursor_info (Syntax.ZENode e) in
    let permitted_actions =
      Environment.Agent.check_actions ActionConv.action_list e
    in
    let action = Random.int (List.length permitted_actions) in
    let action = List.nth permitted_actions action in

    match action with
    | Construct Hole
    | Construct (Const _)
    | Construct (Var _)
    | Construct (Arg _) -> (
        match cursor_info.current_term with
        | ENode e' ->
            (* Forbid actions from erasing subtrees *)
            if num_child e' = 0
            then generate (Environment.Agent.perform_action e action) (n - 1)
            else generate e n
        | TNode _ -> raise (Failure "Invalid action")
        | PNode _ -> raise (Failure "Invalid action"))
    | Construct (PatConst _) | Construct PatVar | Construct PatWild -> (
        match cursor_info.current_term with
        | ENode _ -> raise (Failure "Invalid action")
        | TNode _ -> raise (Failure "Invalid action")
        | PNode p' ->
            (* Forbid actions from erasing subtrees *)
            if num_child_pattern p' = 0
            then generate (Environment.Agent.perform_action e action) (n - 1)
            else generate e n)
    | Unwrap x -> (
        match cursor_info.current_term with
        | ENode e' ->
            (* Forbid actions from erasing subtrees *)
            if check_child e' x
            then generate (Environment.Agent.perform_action e action) (n - 1)
            else generate e n
        | TNode t -> raise (Failure "Invalid action")
        | PNode p ->
            (* Forbid actions from erasing subtrees *)
            if check_child_pattern p x
            then generate (Environment.Agent.perform_action e action) (n - 1)
            else generate e n)
    | _ -> generate (Environment.Agent.perform_action e action) (n - 1)
