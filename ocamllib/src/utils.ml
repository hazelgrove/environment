(* Miscellaneous functions *)
exception IOError of string

let serialize (zast : Expr.z_t) : string =
  Core.Sexp.to_string (Expr.sexp_of_z_t zast)

let deserialize (zast : string) : Expr.z_t =
  Expr.z_t_of_sexp (Core.Sexp.of_string zast)

(* Ranodmly select a root of the tree as the cursor position *)
let select_root_random (e : Expr.t) : Expr.z_t =
  let size = Expr.size e in
  Random.self_init ();
  let index = Random.int size in
  print_int index;
  let rec select_root_index (e : Expr.t) (index : int) : Expr.z_t * int = 
    (* Use -1 as indicator that there is a cursor position found, otherwise return remaining amount of index *)
    if index = 0 then (Expr.select_root e, -1) else
    match e.node with
    | EVar _ | EInt _ | EBool _ | EHole | ENil ->
      (Expr.make_dummy_z_node (Expr.Cursor EHole), index - 1)
    | EUnOp (op, e1) -> 
      let (zast, index) = select_root_index e1 (index - 1) in
      if index = -1
      then (Expr.zip_migrate e (EUnOp_L (op, zast)), -1)
      else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
    | EBinOp (e1, op, e2) ->
      let (zast1, index) = select_root_index e1 (index - 1) in
      if index = -1 
      then (Expr.zip_migrate e (EBinOp_L (zast1, op, e2)), -1)
      else let (zast2, index) = select_root_index e2 index in
      if index = -1
      then (Expr.zip_migrate e (EBinOp_R (e1, op, zast2)), -1)
      else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
    | ELet (x, edef, ebody) ->
      if index = 1 then (Expr.select_root e, -1) else
      let (zast1, index) = select_root_index edef (index - 2) in
      if index = -1 
      then (Expr.zip_migrate e (ELet_L (x, zast1, ebody)), -1)
      else let (zast2, index) = select_root_index ebody index in
      if index = -1
      then (Expr.zip_migrate e (ELet_R (x, edef, zast2)), -1)
      else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
    | EIf (econd, ethen, eelse) ->
      let (zast1, index) = select_root_index econd (index - 1) in
      if index = -1 
      then (Expr.zip_migrate e (EIf_L (zast1, ethen, eelse)), -1)
      else let (zast2, index) = select_root_index ethen index in
      if index = -1
      then (Expr.zip_migrate e (EIf_C (econd, zast2, eelse)), -1)
      else let (zast3, index) = select_root_index eelse index in
      if index = -1
      then (Expr.zip_migrate e (EIf_R (econd, ethen, zast3)), -1)
      else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
    | EFun (x, ty, ebody) -> 
      (* Forbid entering type subtree *)
      if index <= Type.size ty then (Expr.select_root e, -1) else
      let (zast, index) = select_root_index ebody (index - 2) in
      if index = -1
      then (Expr.zip_migrate e (EFun_R (x, ty, zast)), -1)
      else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
    | EFix (x, ty, ebody) -> 
      (* Forbid entering type subtree *)
      if index <= Type.size ty then (Expr.select_root e, -1) else
      let (zast, _) = select_root_index ebody (index - 2) in
      if index = -1
      then (Expr.zip_migrate e (EFix_R (x, ty, zast)), -1)
      else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
    | EPair (e1, e2) ->
      let (zast1, index) = select_root_index e1 (index - 1) in
      if index = -1 
      then (Expr.zip_migrate e (EPair_L (zast1, e2)), -1)
      else let (zast2, _) = select_root_index e2 index in
      if index = -1
      then (Expr.zip_migrate e (EPair_R (e1, zast2)), -1)
      else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
  in
  let (e, _) = select_root_index e index in 
  e


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
