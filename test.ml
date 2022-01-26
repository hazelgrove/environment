(* Testing various functionalities in OCaml *)

Py.initialize ();
let test = Py.import "test" in
let a = 1 in 
let x = Py.Module.get_function test "print_int"
  ([| Py.Int.of_int a |]) in
assert (Py.Int.to_int x = 2)