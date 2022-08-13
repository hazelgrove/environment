(* Generate code n actions away from original *)
let rec generate (e : Expr.z_t) (n : int) : Expr.z_t = 
    let num_child (e : Expr.t) : int = 
        let e = Expr.strip e in
        match e with
        | IntLit _ | BoolLit _ | Hole | Nil | Var _ -> 0
        | BinOp (e1, _, e2) | Pair (e1, e2) | Let (_, e1, e2) -> 
            begin match e1, e2 with
            | Hole, Hole -> 0
            | Hole, _ | _, Hole -> 1
            | _ -> 2
            end
        | If (e1, e2, e3) -> 
            begin match e1, e2, e3 with
            | Hole, Hole, Hole -> 0
            | _, Hole, Hole | Hole, _, Hole | Hole, Hole, _ -> 1
            | Hole, _, _ | _, Hole, _ | _, _, Hole -> 2
            | _, _, _ -> 3
            end
        | Fun (_, _, e) | Fix (_, _, e) | UnOp (_, e) ->
            begin match e with
            | Hole -> 0
            | _ -> 1
            end
    in

    if n = 0 then e else
    let cursor_info = CursorInfo.get_cursor_info (Syntax.ZENode e) in
    let permitted_actions = CursorInfo.cursor_info_to_actions cursor_info in

    Random.self_init ();
    let action = Random.int (List.length permitted_actions) in
    let action = List.nth permitted_actions action in

    match action with
    | Construct Hole | Construct Nil | Construct (Int _) | Construct (Bool _) ->
        begin match cursor_info.current_term with
        | ENode e' ->
            (* Forbid actions from erasing subtrees *)
            if num_child e' = 0 then
                generate (Environment.Agent.perform_action e action) (n - 1)
            else
                generate e n
        | TNode t -> raise (Failure "Invalid action")
        end
    | Unwrap _ ->
        begin match cursor_info.current_term with
        | ENode e' ->
            (* Forbid actions from erasing subtrees *)
            if num_child e' <= 1 then
                generate (Environment.Agent.perform_action e action) (n - 1)
            else
                generate e n
        | TNode t -> raise (Failure "Invalid action")
        end
    | _ ->
        generate (Environment.Agent.perform_action e action) (n - 1)
            