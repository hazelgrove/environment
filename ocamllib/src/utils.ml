(* Miscellaneous functions *)
exception IOError of string

let serialize (zast : Expr.z_t) : string =
  Core.Sexp.to_string (Expr.sexp_of_z_t zast)

let deserialize (zast : string) : Expr.z_t =
  Expr.z_t_of_sexp (Core.Sexp.of_string zast)

let select_root_index (e : Expr.t) (index : int) : Expr.z_t =
  let rec select_root_index_aux (e : Expr.t) (index : int) : Expr.z_t * int =
    (* Use -1 as indicator that there is a cursor position found, otherwise return remaining amount of index *)
    let rec select_root_pattern (p : Pattern.t) (index : int) :
        Pattern.z_t * int =
      if index = 0
      then (Pattern.select_root p, -1)
      else
        match p.node with
        | PConst _ | PVar _ | PWild ->
            (Pattern.make_dummy_z_node (Pattern.Cursor PWild), index - 1)
        | PCons (p1, p2) ->
            let zast1, index = select_root_pattern p1 (index - 1) in
            if index = -1
            then (Pattern.zip_migrate p (PCons_L (zast1, p2)), -1)
            else
              let zast2, index = select_root_pattern p2 index in
              if index = -1
              then (Pattern.zip_migrate p (PCons_R (p1, zast2)), -1)
              else (Pattern.make_dummy_z_node (Pattern.Cursor PWild), index)
    in
    if index = 0
    then (Expr.select_root e, -1)
    else
      match e.node with
      | EVar _ | EConst _ | EHole ->
          (Expr.make_dummy_z_node (Expr.Cursor EHole), index - 1)
      | EUnOp (op, e1) ->
          let zast, index = select_root_index_aux e1 (index - 1) in
          if index = -1
          then (Expr.zip_migrate e (EUnOp_L (op, zast)), -1)
          else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EBinOp (e1, op, e2) ->
          let zast1, index = select_root_index_aux e1 (index - 1) in
          if index = -1
          then (Expr.zip_migrate e (EBinOp_L (zast1, op, e2)), -1)
          else
            let zast2, index = select_root_index_aux e2 index in
            if index = -1
            then (Expr.zip_migrate e (EBinOp_R (e1, op, zast2)), -1)
            else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | ELet (x, edef, ebody) ->
          if index = 1
          then (Expr.select_root e, -1)
          else
            let zast1, index = select_root_index_aux edef (index - 2) in
            if index = -1
            then (Expr.zip_migrate e (ELet_L (x, zast1, ebody)), -1)
            else
              let zast2, index = select_root_index_aux ebody index in
              if index = -1
              then (Expr.zip_migrate e (ELet_R (x, edef, zast2)), -1)
              else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EIf (econd, ethen, eelse) ->
          let zast1, index = select_root_index_aux econd (index - 1) in
          if index = -1
          then (Expr.zip_migrate e (EIf_L (zast1, ethen, eelse)), -1)
          else
            let zast2, index = select_root_index_aux ethen index in
            if index = -1
            then (Expr.zip_migrate e (EIf_C (econd, zast2, eelse)), -1)
            else
              let zast3, index = select_root_index_aux eelse index in
              if index = -1
              then (Expr.zip_migrate e (EIf_R (econd, ethen, zast3)), -1)
              else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EFun (x, ty, ebody) ->
          (* Forbid entering type subtree *)
          let t_size = Type.size ty in
          if index <= t_size + 1
          then (Expr.select_root e, -1)
          else
            let zast, index =
              select_root_index_aux ebody (index - 2 - t_size)
            in
            if index = -1
            then (Expr.zip_migrate e (EFun_R (x, ty, zast)), -1)
            else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EFix (x, ty, ebody) ->
          (* Forbid entering type subtree *)
          if index <= Type.size ty
          then (Expr.select_root e, -1)
          else
            let zast, index = select_root_index_aux ebody (index - 2) in
            if index = -1
            then (Expr.zip_migrate e (EFix_R (x, ty, zast)), -1)
            else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EPair (e1, e2) ->
          let zast1, index = select_root_index_aux e1 (index - 1) in
          if index = -1
          then (Expr.zip_migrate e (EPair_L (zast1, e2)), -1)
          else
            let zast2, index = select_root_index_aux e2 index in
            if index = -1
            then (Expr.zip_migrate e (EPair_R (e1, zast2)), -1)
            else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EMap (e1, e2) ->
          let zast1, index = select_root_index_aux e1 (index - 1) in
          if index = -1
          then (Expr.zip_migrate e (EMap_L (zast1, e2)), -1)
          else
            let zast2, index = select_root_index_aux e2 index in
            if index = -1
            then (Expr.zip_migrate e (EMap_R (e1, zast2)), -1)
            else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EFilter (e1, e2) ->
          let zast1, index = select_root_index_aux e1 (index - 1) in
          if index = -1
          then (Expr.zip_migrate e (EFilter_L (zast1, e2)), -1)
          else
            let zast2, index = select_root_index_aux e2 index in
            if index = -1
            then (Expr.zip_migrate e (EFilter_R (e1, zast2)), -1)
            else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EListEq (e1, e2) ->
          let zast1, index = select_root_index_aux e1 (index - 1) in
          if index = -1
          then (Expr.zip_migrate e (EListEq_L (zast1, e2)), -1)
          else
            let zast2, index = select_root_index_aux e2 index in
            if index = -1
            then (Expr.zip_migrate e (EListEq_R (e1, zast2)), -1)
            else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EFold (efunc, eacc, elist) ->
          let zast1, index = select_root_index_aux efunc (index - 1) in
          if index = -1
          then (Expr.zip_migrate e (EFold_L (zast1, eacc, elist)), -1)
          else
            let zast2, index = select_root_index_aux eacc index in
            if index = -1
            then (Expr.zip_migrate e (EFold_C (efunc, zast2, elist)), -1)
            else
              let zast3, index = select_root_index_aux elist index in
              if index = -1
              then (Expr.zip_migrate e (EFold_R (efunc, eacc, zast3)), -1)
              else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EMatch (e, (p1, e1), (p2, e2)) ->
          let zast1, index = select_root_index_aux e (index - 1) in
          if index = -1
          then (Expr.zip_migrate e (EMatch_L (zast1, (p1, e1), (p2, e2))), -1)
          else
            let zast2, index = select_root_pattern p1 index in
            if index = -1
            then (Expr.zip_migrate e (EMatch_P1 (e, (zast2, e1), (p2, e2))), -1)
            else
              let zast3, index = select_root_index_aux e1 index in
              if index = -1
              then
                (Expr.zip_migrate e (EMatch_E1 (e, (p1, zast3), (p2, e2))), -1)
              else
                let zast4, index = select_root_pattern p2 index in
                if index = -1
                then
                  (Expr.zip_migrate e (EMatch_P2 (e, (p1, e1), (zast4, e2))), -1)
                else
                  let zast5, index = select_root_index_aux e2 index in
                  if index = -1
                  then
                    ( Expr.zip_migrate e (EMatch_E2 (e, (p1, e1), (p2, zast5))),
                      -1 )
                  else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
      | EAssert e1 ->
          let zast, index = select_root_index_aux e1 (index - 1) in
          if index = -1
          then (Expr.zip_migrate e (EAssert_L zast), -1)
          else (Expr.make_dummy_z_node (Expr.Cursor EHole), index)
  in
  if index >= Expr.size e
  then raise (Failure "Index out of bound")
  else
    let e, _ = select_root_index_aux e index in
    e

(* Ranodmly select a root of the tree as the cursor position *)
let select_root_random (e : Expr.t) : Expr.z_t =
  let size = Expr.size e in
  let index = Random.int size in
  select_root_index e index

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
    | Const Nil -> []
    | BinOp (Pair (Const (Int a), Const (Int b)), OpCons, tl) ->
        (a, b) :: combine_tests tl
    | _ -> raise (IOError "Test file in incorrect format.")
  in
  combine_tests tests_cons

(* Given an assignment number, load the code
   Input:
     - assignment : index of assignment
   Output:
     - the code for the assignment
*)
let load_starter_code (directory : string) (assignment : int) (index : int)
    (cursor : int option) : Expr.z_t =
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
            id = e.id;
            node = Expr.EFun_R (x, Type.set_starter ty true, find_fun_body e);
            starter = true;
          }
        in
        e
    | _ -> (
        match cursor with
        | None -> select_root_random e
        | Some i -> select_root_index e i)
  in
  let rec find_fun_def (e : Expr.t) : Expr.z_t =
    (* Assumes that there will only be lets before the function definition *)
    match e.node with
    | ELet (x, edef, ebody) ->
        if Var.equal x Var.starter_func
        then
          {
            id = e.id;
            node =
              ELet_L
                ( Var.starter_func,
                  find_fun_body edef,
                  Expr.set_starter ebody true );
            starter = true;
          }
        else
          {
            id = e.id;
            node = ELet_R (x, Expr.set_starter edef true, find_fun_def ebody);
            starter = true;
          }
    | _ -> raise (IOError "Starter code file in incorrect format.")
  in
  find_fun_def (Expr.add_metadata e)
