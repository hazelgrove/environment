(* Used for manual interaction with environemnt *)
open Environment.Agent

let get_action : unit -> Action.t option =
 fun _ ->
  let action = read_int () in
  try Some (ActionConv.tag_to_action action) with Failure _ -> None

let rec interactive_loop (ast : Expr.z_t) (action : Action.t option) : unit =
  let ast =
    match action with Some act -> perform_action ast act | None -> ast
  in
  print_zast ast 0;
  print_endline "";
  print_string "Action Index: ";
  let next_action = get_action () in
  interactive_loop ast next_action

let () =
  interactive_loop
    (Cursor
       (ParserUtils.parse "if true then (fun x -> x + 1) else (fun x -> 2 * x)"))
    None
