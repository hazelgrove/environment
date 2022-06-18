open Bigarray
open Environment
open ArrayUtils

(* Get nodes array from C *)
external get_nodes : unit -> (int32, int32_elt, c_layout) Array1.t
  = "pass_nodes"

(* Pass nodes array to C *)
external pass_nodes : (int32, int32_elt, c_layout) Array1.t -> unit
  = "get_nodes"

(* Get edges array from C *)
external get_edges : unit -> (int32, int32_elt, c_layout) Array2.t
  = "pass_edges"

(* Pass edges array to C *)
external pass_edges : (int32, int32_elt, c_layout) Array2.t -> unit
  = "get_edges"

external pass_vars_in_scope : (int32, int32_elt, c_layout) Array1.t -> unit
  = "get_vars_in_scope"

(* Get unit tests from C *)
external get_unit_tests : unit -> (int32, int32_elt, c_layout) Array2.t
  = "pass_unit_tests"

(* Pass unit tests to C *)
external pass_unit_tests : (int32, int32_elt, c_layout) Array2.t -> unit
  = "get_unit_tests"

external pass_actions : (int32, int32_elt, c_layout) Array1.t -> unit
  = "get_actions"

(* Called by C. Changes zast according to the action *)
let change_zast_c (ser_zast : string) (action : int) : string =
  let zast = Utils.deserialize ser_zast in
  let action = ActionConv.tag_to_action action in
  let zast = Agent.change_ast zast action in
  Utils.serialize zast

(* Update the observation for the given zast *)
let get_ast_c (ser_zast : string) : unit =
  let zast = Utils.deserialize ser_zast in
  let (nodes, edges), _ = ExprConv.to_list zast in
  pass_nodes (list_to_array1 nodes);
  pass_edges (list_to_edge edges);
  ()

let get_cursor_info_c (ser_zast : string) : int =
  let zast = Utils.deserialize ser_zast in
  let (nodes, edges), cursorInfo = ExprConv.to_list zast in
  let actions =
    cursorInfo |> CursorInfo.cursor_info_to_actions |> ActionConv.to_list
    |> List.map (fun b -> if b then 1 else 0)
  in
  let vars_in_scope =
    List.map (fun (var, _) -> ExprConv.node_to_tag (EVar var)) cursorInfo.ctx
  in
  pass_actions (list_to_array1 actions);
  pass_vars_in_scope (list_to_array1 vars_in_scope);
  CursorInfo.get_cursor_position (Syntax.ZENode zast)

(* run_unittests function that will be called by C *)
let run_unit_tests_c (root : int) : bool =
  let tests = tests_to_list (get_unit_tests ()) in
  let nodes = array1_to_list (get_nodes ()) in
  let edges = edge_to_list (get_edges ()) in
  let e = ExprConv.from_list nodes edges root in
  Evaluator.run_unit_tests tests e

(* load_assignment function that will be called by C *)
let load_tests_c (assignment : int) : unit =
  let unit_tests = Utils.load_tests "data" assignment in
  pass_unit_tests (list_to_tests unit_tests)

(* load_assignment function that will be called by C *)
let load_starter_code_c (assignment : int) (index : int) : string =
  let e = Utils.load_starter_code "data" assignment index in
  let zast = Expr.select_root e in
  Utils.serialize zast

(* For debugging use *)
let print_code_c (root : int) : unit =
  let nodes = array1_to_list (get_nodes ()) in
  let edges = edge_to_list (get_edges ()) in
  let e = ExprConv.from_list nodes edges root in
  let s = ExprConv.to_string e in
  let _ = Sys.command ("echo '" ^ s ^ "' | ocamlformat - --impl") in
  ()

let _ = Callback.register "run_unit_tests" run_unit_tests_c
let _ = Callback.register "change_zast" change_zast_c
let _ = Callback.register "get_ast" get_ast_c
let _ = Callback.register "get_cursor_info" get_cursor_info_c
let _ = Callback.register "load_tests" load_tests_c
let _ = Callback.register "load_starter_code" load_starter_code_c
let _ = Callback.register "print_code" print_code_c
