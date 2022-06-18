(* Miscellaneous functions *)
exception IOError of string

let serialize (zast : Expr.z_t) : string =
  Core.Sexp.to_string (Expr.sexp_of_z_t zast)

let deserialize (zast : string) : Expr.z_t =
  Expr.z_t_of_sexp (Core.Sexp.of_string zast)

(* Given an assignment number, load the unit test
   Input:
     - assignment : index of assignment
   Output:
     - the unit test for the assignment
*)
let load_tests (directory : string) (assignment : int) : (int * int) list =
  let filename = directory ^ "/" ^ string_of_int assignment ^ "/test.ml" in
  let tests_cons = ParserUtils.parse_file filename in
  let rec combine_tests (tests_cons : Expr.t) : (int * int) list =
    match tests_cons with
    | EBinOp (EPair (EInt a, EInt b), OpCons, ENil) -> [ (a, b) ]
    | EBinOp (EPair (EInt a, EInt b), OpCons, tl) -> (a, b) :: combine_tests tl
    | _ -> raise (IOError "Test file in incorrect format.")
  in
  combine_tests tests_cons

(* Given an assignment number, load the code
   Input:
     - assignment : index of assignment
   Output:
     - the code for the assignment
*)
let load_starter_code (directory : string) (assignment : int) (index : int) :
    Expr.t =
  let filename =
    directory ^ "/" ^ string_of_int assignment ^ "/" ^ string_of_int index
    ^ ".ml"
  in
  ParserUtils.parse_file filename
