module Astparser = struct

let read_sig filename =
  Location.input_name := filename ;
  let handle =
    try open_in filename
    with Sys_error msg -> prerr_endline msg; exit 1
  in
  let buf = Lexing.from_channel handle in
  Location.init buf filename ;
  let ast = Parse.implementation buf in
  close_in handle ;
  ast

let write_struct filename ast =
  let handle =
    try open_out filename
    with Sys_error msg -> prerr_endline msg; exit 1
  in
  let fmt = Format.formatter_of_out_channel handle in
  Format.fprintf fmt "%a@." Pprintast.structure ast ;
  close_out handle
end

(* let () =
  if Array.length Sys.argv < 3 then begin
    Printf.eprintf "Usage: %s ML RESULT EXPECTED\n" Sys.argv.(0);
    exit 2
  end;

  let ml_file = Sys.argv.(1) in
  let result_file = Sys.argv.(2) in

  let () =
    read_sig ml_file |> write_struct result_file
  in

  let s = Printf.sprintf
      "test"
  in

  exit @@ Sys.command s *)