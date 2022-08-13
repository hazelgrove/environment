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
  let rec combine_tests (tests_cons : Expr.p_t) : (int * int) list =
    match tests_cons with
    | BinOp (Pair (IntLit a, IntLit b), OpCons, Nil) -> [ (a, b) ]
    | BinOp (Pair (IntLit a, IntLit b), OpCons, tl) ->
        (a, b) :: combine_tests tl
    | _ -> raise (IOError "Test file in incorrect format.")
  in
  combine_tests (Expr.strip tests_cons)

(* Given an assignment number, load the code
   Input:
     - assignment : index of assignment
   Output:
     - the code for the assignment
*)
let load_starter_code (directory : string) (assignment : int) (index : int) :
    Expr.z_t =
  let filename =
    directory ^ "/" ^ string_of_int assignment ^ "/" ^ string_of_int index
    ^ ".ml"
  in
  let e = ParserUtils.parse_file filename in
  let rec find_fun_body (e : Expr.t) : Expr.z_t = 
    match e.node with
    | Expr.EFun (x, ty, e) ->
      let e : Expr.z_t = 
        {
          id=e.id;
          node=Expr.EFun_R (x, ty, find_fun_body e);
          starter=true;
        }
      in
      e
    | _ -> Expr.select_root e
  in
  match e.node with
  | ELet (x, edef, ebody) ->
    {
      id=e.id;
      node=ELet_L (x, find_fun_body edef, ebody);
      starter=true;
    }
  | _ -> raise (Failure "Starter code in incorect format")
