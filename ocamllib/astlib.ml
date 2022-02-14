open Bigarray

external get_nodes: unit -> (int, int64_elt, c_layout) Array1.t = "pass_nodes"
external pass_nodes: (int, int64_elt, c_layout) Array1.t -> unit = "get_nodes"

let change_node (action : int) : unit =
  let nodes = get_nodes () in
  if nodes.{action} = 0 then 
    (* let () = Array1.set nodes action 1 in pass_nodes nodes *)
    pass_nodes nodes
  else
    (* let () = Array1.set nodes action 0 in pass_nodes nodes *)
    pass_nodes nodes


let evaluate_ast (n : int) : int = if n = 2 then 1 else 0

let _ = Callback.register "evaluate_ast" evaluate_ast
let _ = Callback.register "change_node" change_node