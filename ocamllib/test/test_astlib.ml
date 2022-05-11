open OUnit2
open Astlib
open Ast
open Astutil

(* Test change_ast *)

(* Test run_unit_tests *)
let test_empty_code _ =
  let test_set = [ (1, 2); (2, 3); (-2, -1) ] in
  let code = Expr.EHole in
  assert_equal false (run_unit_tests test_set code)

let test_empty_tests _ =
  let test_set = [] in
  let code = Expr.EHole in
  assert_equal true (run_unit_tests test_set code)

let test_invalid_code _ =
  let test_set = [ (1, 2); (2, 3); (-2, -1) ] in
  let code = parse "fun x -> x + 1" in
  assert_equal false (run_unit_tests test_set code)

let test_pass_all _ =
  let test_set = [ (1, 2); (2, 3); (-2, -1) ] in
  let code = parse "let y x = x + 1" in
  assert_equal true (run_unit_tests test_set code);

  let test_set = [ (1, 2); (2, 4); (-1, -2); (0, 0) ] in
  let code = parse "let y x = x * 2" in
  assert_equal true (run_unit_tests test_set code)

let test_pass_partial _ =
  let test_set = [ (1, 3); (2, 3); (-2, 0) ] in
  let code = parse "let y x = x + 1" in
  assert_equal false (run_unit_tests test_set code);

  let test_set = [ (1, 2); (2, 4); (-1, -2); (0, 0) ] in
  let code = parse "let y x = x * 3" in
  assert_equal false (run_unit_tests test_set code)

let test_fail_all _ =
  let test_set = [ (1, 3); (2, 4); (-2, 0) ] in
  let code = parse "let y x = x + 1" in
  assert_equal false (run_unit_tests test_set code);

  let test_set = [ (1, 2); (2, 4); (-1, -2); (0, 4) ] in
  let code = parse "let y x = x * 3" in
  assert_equal false (run_unit_tests test_set code)

let test_move_parent =
  (* all move downs *)
  [
    "Move Parent base"
    >:: assert_equal (Cursor ENull) (change_ast (Cursor ENull) (Move Parent));
    "Move Parent BinOpL"
    >:: assert_equal
          (Cursor (EBinOp (EInt 2, OpPlus, EInt 3)))
          (change_ast
             (EBinOp_L (Cursor (EInt 2), OpPlus, EInt 3))
             (Move Parent));
    "Move Parent BinOpR"
    >:: assert_equal
          (Cursor (EBinOp (EInt 2, OpDiv, EInt 3)))
          (change_ast (EBinOp_R (EInt 2, OpDiv, Cursor (EInt 3))) (Move Parent));
    "Move Parent UnOp"
    >:: assert_equal
          (Cursor (EUnOp (OpNeg, EBool false)))
          (change_ast (EUnop_L (OpNeg, Cursor (EBool false))) (Move Parent));
    "Move Parent Let_L"
    >:: assert_equal
          (Cursor (ELet ("var", EInt 2, ENull)))
          (change_ast (ELet_L ("var", Cursor (EInt 2), ENull)) (Move Parent));
    "Move Parent Let_R"
    >:: assert_equal
          (Cursor (ELet ("var", EInt 2, ENull)))
          (change_ast (ELet_R ("var", EInt 2, Cursor ENull)) (Move Parent));
    "Move Parent If_L"
    >:: assert_equal
          (Cursor (EIF (EBool false, EBinop (EInt 3, OpPlus, EInt 2), EInt 3)))
          (change_ast
             (EIF_L
                (Cursor (EBool false), EBinop (EInt 3, OpPlus, EInt 2), EInt 3))
             (Move Parent));
    "Move Parent If_C"
    >:: assert_equal
          (Cursor (EIF (EBool false, EBinop (EInt 3, OpPlus, EInt 2), EInt 3)))
          (change_ast
             (EIF_C
                (EBool false, Cursor (EBinop (EInt 3, OpPlus, EInt 2)), EInt 3))
             (Move Parent));
    "Move Parent If_R"
    >:: assert_equal
          (Cursor (EIF (EBool false, EBinop (EInt 3, OpPlus, EInt 2), EInt 3)))
          (change_ast
             (EIF_L
                (EBool false, EBinop (EInt 3, OpPlus, EInt 2), Cursor (EInt 3)))
             (Move Parent));
    "Move Parent EFun_L"
    >:: assert_equal
          (Cursor (EFun ("var", TBool, EUnOp (OpNeg, EVar "var"))))
          (change_ast
             (EFun_L ("var", Cursor TBool, EUnOp (OpNeg, EVar "var")))
             (Move Parent));
    "Move Parent EFun_R"
    >:: assert_equal
          (Cursor (EFun ("var", TBool, EUnOp (OpNeg, EVar "var"))))
          (change_ast
             (EFun_L ("var", TBool, Cursor (EUnOp (OpNeg, EVar "var"))))
             (Move Parent));
    "Move Parent EFix_L"
    >:: assert_equal
          (Cursor (EFix ("var", TBool, EUnOp (OpNeg, EVar "var"))))
          (change_ast
             (EFix_L ("var", Cursor TBool, EUnOp (OpNeg, EVar "var")))
             (Move Parent));
    "Move Parent EFix_R"
    >:: assert_equal
          (Cursor (EFix ("var", TBool, EUnOp (OpNeg, EVar "var"))))
          (change_ast
             (EFix_L ("var", TBool, Cursor (EUnOp (OpNeg, EVar "var"))))
             (Move Parent));
    "Move Parent EPair_L"
    >:: assert_equal
          (Cursor (EPair (EBool false, EInt 3)))
          (change_ast (EPair_L (Cursor (EBool false), EInt 3)) (Move Parent));
    "Move Parent EPair_R "
    >:: assert_equal
          (Cursor (EPair (EBool false, EInt 3)))
          (change_ast (EPair_R (EBool false, Cursor (EInt 3))) (Move Parent));
    (* test ability to 'recurse through' each type *)
    "Move Parent Recurse UnopL"
    >:: assert_equal
          (EUnOp_L (OpNeg, Cursor (EUnOp (OpNeg, EHole))))
          (change_ast
             (EUnOp_L (OpNeg, EUnop_L (OpNeg, Cursor EHole)))
             (Move Parent));
    "Move Parent Recurse BinopL"
    >:: assert_equal
          (EBinOp_L (Cursor (EUnOp (OpNeg, EHole)), OpLe, EHole))
          (change_ast
             (EBinOp_L (EUnop_L (OpNeg, Cursor EHole), OpLe, EHole))
             (Move Parent));
    "Move Parent Recurse BinopR"
    >:: assert_equal
          (EBinOp_R (EHole, OpGt (Cursor (EUnOp (OpNeg, EHole)))))
          (change_ast
             (EBinOp_R (EHole, OpGt (EUnop_L (OpNeg, Cursor EHole))))
             (Move Parent));
    "Move Parent Recurse Let_L"
    >:: assert_equal
          (ELet_L ("name", Cursor (EUnOp (OpNeg, EHole)), EHole))
          (change_ast
             (ELet_L ("name", EUnop_L (OpNeg, Cursor EHole), EHole))
             (Move Parent));
    "Move Parent Recurse Let_R"
    >:: assert_equal
          (ELet_R ("name", EHole, Cursor (EUnOp (OpNeg, EHole))))
          (change_ast
             (ELet_R ("name", EHole, EUnop_L (OpNeg, Cursor EHole)))
             (Move Parent));
    "Move Parent Recurse If_L"
    >:: assert_equal
          (EIf_L (Cursor (EUnOp (OpNeg, EHole)), EHole, ENil))
          (change_ast
             (EIf_L (EUnop_L (OpNeg, Cursor EHole)), EHole, ENil)
             (Move Parent));
    "Move Parent Recurse If_C"
    >:: assert_equal
          (EIf_C (EBool false, Cursor (EUnOp (OpNeg, EHole)), EHole))
          (change_ast
             (EIf_C (EBool false, EUnop_L (OpNeg, Cursor EHole), EHole))
             (Move Parent));
    "Move Parent Recurse If_R"
    >:: assert_equal
          (EIf_R (EBool false, EHole, Cursor (EUnOp (EUnOp (OpNeg, EHole)))))
          (change_ast
             (EIf_R (EBool false, EHole, EUnop_L (OpNeg, Cursor EHole)))
             (Move Parent));
    "Move Parent Recurse Fun_R"
    >:: assert_equal
          (EFun_R ("var", TBool, Cursor (EUnOp (OpNeg, EHole))))
          (change_ast
             (EFun_R ("var", TBool, EUnOp_L (OpNeg, Cursor EHole)))
             (Move Parent));
    "Move Parent Recurse Fun_L"
    >:: assert_equal
          (EFun_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
          (change_ast
             (EFun_R ("var", TList (Cursor TBool), EUnOp_L (OpNeg, EHole)))
             (Move Parent));
    "Move Parent Fun_child"
    >:: assert_equal
          (Cursor (EFun ("var", TList TBool, EUnOp (OpNeg, EHole))))
          (change_ast
             (EFun_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
             (Move Parent));
    "Move Parent Recurse Fix_R"
    >:: assert_equal
          (EFix_R ("var", TBool, Cursor (EUnOp (OpNeg, EHole))))
          (change_ast
             (EFix_R ("var", TBool, EUnOp_L (OpNeg, Cursor EHole)))
             (Move Parent));
    "Move Parent Recurse Fix_L"
    >:: assert_equal
          (EFix_L ("var", Cursor (TList TBool), EHole))
          (change_ast
             (EFix_R ("var", TList (Cursor TBool), EHole))
             (Move Parent));
    "Move Parent fix_child"
    >:: assert_equal
          (Cursor (EFix ("var", TList TBool, EUnOp (OpNeg, EHole))))
          (change_ast
             (EFix_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
             (Move Parent));
    "Move Parent Recurse Pair_L"
    >:: assert_equal
          (EPair_L (Cursor (EUnOp (OpNeg, EHole))), ENil)
          (change_ast
             (EPair_L (EUnOp_L (OpNeg, Cursor EHole), EHole))
             (Move Parent))
    (* "Move Parent Recurse Pair_R" >:: assert_equal (EPair_R (ENil,Cursor (EUnOp (OpNeg, EHole))) ) (change_ast (EPair_R EHole, (EUnOp_L (OpNeg, Cursor(EHole)) )) (Move Parent)); *)
    (* "gotchya" >:: assert_equal true false *);
  ]

let test_move_child =
  (* all move downs *)
  [
    "Move Child BinOpL"
    >:: assert_equal
          (EBinOp_L (Cursor (EInt 2), OpPlus, EInt 3))
          (change_ast
             (Cursor (EBinOp (EInt 2, OpPlus, EInt 3)))
             (Move (Child 0)));
    "Move Child BinOpR"
    >:: assert_equal
          (EBinOp_R (EInt 2, OpDiv, Cursor (EInt 3)))
          (change_ast
             (Cursor (EBinOp (EInt 2, OpDiv, EInt 3)))
             (Move (Child 1)));
    "Move Child UnOp"
    >:: assert_equal
          (EUnop_L (OpNeg, Cursor (EBool false)))
          (change_ast (Cursor (EUnOp (OpNeg, EBool false))) (Move (Child 0)));
    "Move Child Let_L"
    >:: assert_equal
          (ELet_L ("var", Cursor (EInt 2), ENull))
          (change_ast (Cursor (ELet ("var", EInt 2, ENull))) (Move (Child 0)));
    "Move Child Let_R"
    >:: assert_equal
          (ELet_R ("var", EInt 2, Cursor ENull))
          (change_ast (Cursor (ELet ("var", EInt 2, ENull))) (Move (Child 1)));
    "Move Child If_L"
    >:: assert_equal
          (EIF_L (Cursor (EBool false), EBinop (EInt 3, OpPlus, EInt 2), EInt 3))
          (change_ast
             (Cursor
                (EIF (EBool false, EBinop (EInt 3, OpPlus, EInt 2), EInt 3)))
             (Move (Child 0)));
    "Move Child If_C"
    >:: assert_equal
          (EIF_C (EBool false, Cursor (EBinop (EInt 3, OpPlus, EInt 2)), EInt 3))
          (change_ast
             (Cursor
                (EIF (EBool false, EBinop (EInt 3, OpPlus, EInt 2), EInt 3)))
             (Move (Child 1)));
    "Move Child If_R"
    >:: assert_equal
          (EIF_L (EBool false, EBinop (EInt 3, OpPlus, EInt 2), Cursor (EInt 3)))
          (change_ast
             (Cursor
                (EIF (EBool false, EBinop (EInt 3, OpPlus, EInt 2), EInt 3)))
             (Move (Child 2)));
    "Move Child EFun_L"
    >:: assert_equal
          (EFun_L ("var", Cursor TBool, EUnOp (OpNeg, EVar "var")))
          (change_ast
             (Cursor (EFun ("var", TBool, EUnOp (OpNeg, EVar "var"))))
             (Move (Child 0)));
    "Move Child EFun_R"
    >:: assert_equal
          (EFun_L ("var", TBool, Cursor (EUnOp (OpNeg, EVar "var"))))
          (change_ast
             (Cursor (EFun ("var", TBool, EUnOp (OpNeg, EVar "var"))))
             (Move (Child 1)));
    "Move Child EFix_L"
    >:: assert_equal
          (EFix_L ("var", Cursor TBool, EUnOp (OpNeg, EVar "var")))
          (change_ast
             (Cursor (EFix ("var", TBool, EUnOp (OpNeg, EVar "var"))))
             (Move (Child 0)));
    "Move Child EFix_R"
    >:: assert_equal
          (EFix_L ("var", TBool, Cursor (EUnOp (OpNeg, EVar "var"))))
          (change_ast
             (Cursor (EFix ("var", TBool, EUnOp (OpNeg, EVar "var"))))
             (Move (Child 1)));
    "Move Child EPair_L"
    >:: assert_equal
          (EPair_L (Cursor (EBool false), EInt 3))
          (change_ast (Cursor (EPair (EBool false, EInt 3))) (Move (Child 0)));
    "Move Child EPair_R"
    >:: assert_equal
          (EPair_R (EBool false, Cursor (EInt 3)))
          (change_ast (Cursor (EPair (EBool false, EInt 3))) (Move (Child 1)));
    (* test ability to 'recurse through' each type *)
    "Move Child Recurse UnopL"
    >:: assert_equal
          (EUnOp_L (OpNeg, EUnop_L (OpNeg, Cursor EHole)))
          (change_ast
             (EUnOp_L (OpNeg, Cursor (EUnOp (OpNeg, EHole))))
             (Move (Child 0)));
    "Move Child Recurse BinopL"
    >:: assert_equal
          (EBinOp_L (EUnop_L (OpNeg, Cursor EHole), OpLe, EHole))
          (change_ast
             (EBinOp_L (Cursor (EUnOp (OpNeg, EHole)), OpLe, EHole))
             (Move (Child 0));
           "Move Child Recurse BinopR"
           >:: assert_equal
                 (EBinOp_R (EHole, OpGt (EUnop_L (OpNeg, Cursor EHole))))
                 (change_ast
                    (EBinOp_R (EHole, OpGt (Cursor (EUnOp (OpNeg, EHole)))))
                    (Move (Child 0)));
           "Move Child Recurse Let_L"
           >:: assert_equal
                 (ELet_L ("name", EUnop_L (OpNeg, Cursor EHole), EHole))
                 (change_ast
                    (ELet_L ("name", Cursor (EUnOp (OpNeg, EHole)), EHole))
                    (Move (Child 0)));
           "Move Child Recurse Let_R"
           >:: assert_equal
                 (ELet_R ("name", EHole, EUnop_L (OpNeg, Cursor EHole)))
                 (change_ast
                    (ELet_R ("name", EHole, Cursor (EUnOp (OpNeg, EHole))))
                    (Move (Child 0)));
           "Move Child Recurse If_L"
           >:: assert_equal (EIf_L (EUnop_L (OpNeg, Cursor EHole)), EHole, ENil))
          (change_ast
             (EIf_L (Cursor (EUnOp (OpNeg, EHole)), EHole, ENil))
             (Move (Child 0)));
    "Move Child Recurse If_C"
    >:: assert_equal
          (EIf_C (EBool false, EUnop_L (OpNeg, Cursor EHole)), EHole)
          (change_ast
             (EIf_C (EBool false, Cursor (EUnOp (OpNeg, EHole)), EHole))
             (Move (Child 0)));
    "Move Child Recurse If_R"
    >:: assert_equal
          (EIf_R (EBool false, EHole, EUnop_L (OpNeg, Cursor EHole)))
          (change_ast
             (EIf_R (EBool false, EHole, Cursor (EUnOp (EUnOp (OpNeg, EHole)))))
             (Move (Child 0)));
    "Move Child Recurse Fun_R"
    >:: assert_equal
          (EFun_R ("var", TBool, EUnOp_L (OpNeg, Cursor EHole)))
          (change_ast
             (EFun_R ("var", TBool, Cursor (EUnOp (OpNeg, EHole))))
             (Move (Child 0)));
    "Move Child Recurse Fun_L"
    >:: assert_equal
          (EFun_R ("var", TList (Cursor TBool), EUnOp_L (OpNeg, EHole)))
          (change_ast
             (EFun_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
             (Move (Child 0)));
    "Move Child Fun_child"
    >:: assert_equal
          (EFun_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
          (change_ast
             (Cursor (EFun ("var", TList TBool, EUnOp (OpNeg, EHole))))
             (Move (Child 0)));
    "Move Child Recurse Fix_R"
    >:: assert_equal
          (EFix_R ("var", TBool, EUnOp_L (OpNeg, Cursor EHole)))
          (change_ast
             (EFix_R ("var", TBool, Cursor (EUnOp (OpNeg, EHole))))
             (Move (Child 0)));
    "Move Child Recurse Fix_L"
    >:: assert_equal
          (EFix_R ("var", TList (Cursor TBool), EUnOp_L (OpNeg, EHole)))
          (change_ast
             (EFix_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
             (Move (Child 0)));
    "Move Child Fix_child"
    >:: assert_equal
          (EFix_L ("var", Cursor (TList TBool), EUnOp (OpNeg, EHole)))
          (change_ast
             (Cursor (EFix ("var", TList TBool, EUnOp (OpNeg, EHole))))
             (Move (Child 0)));
    "Move Child Recurse Pair_L"
    >:: assert_equal
          (EPair_L (EUnOp_L (OpNeg, Cursor EHole), EHole))
          (change_ast
             (EPair_L (Cursor (EUnOp (OpNeg, EHole))), ENil)
             (Move (Child 0)));
    "Move Child Recurse Pair_R"
    >:: assert_equal
          (EPair_R EHole, EUnOp_L (OpNeg, Cursor EHole))
          (change_ast
             (EPair_R (ENil (Cursor (EUnOp (OpNeg, EHole)))))
             (Move (Child 0)))
    (* "gotchya" >:: assert_equal true false *);
  ]

let test_construct_expr =
  [
    "Test construct var x "
    >:: assert_equal
          (change_ast (Cursor EHole) (Construct (Var "x")))
          (Cursor (EVar "x"));
    "Test construct var y "
    >:: assert_equal
          (change_ast (Cursor EHole) (Construct (Var "y")))
          (Cursor (EVar "y"));
    "Test construct Hole "
    >:: assert_equal
          (change_ast (EUnop_L OpNeg, Cursor ENil) (Construct Hole))
          (EUnop_L OpNeg, Cursor EHole);
    "Construct nil "
    >:: assert_equal
          (change_ast (EBinOp_R (EInt 2), OpGt, Cursor EHole) (Construct Nil))
          (EBinOp_L (EInt 2, OpNeg, Cursor ENil));
    "Construct int "
    >:: assert_equal
          (change_ast
             (EBinOp_L (Cursor EHole), OpEq, EInt 2)
             (Construct (Int 2)))
          (EBinOp_L (Cursor (EInt 4)), OpEq, EInt 2);
    "Construct Bool "
    >:: assert_equal
          (change_ast ELet_L
             ("varname", Cursor EHole, EInt 2)
             (Construct (Bool false)))
          (ELet_L "varname", Cursor (EBool false), EInt 2);
    "Construct UnOp "
    >:: assert_equal
          (change_ast
             (ELet_R ("varname", EInt 2, Cursor (EVar "myvar")))
             (Construct (UnOp OpNeg)))
          (ELet_R ("varname", EInt 2, Cursor (UnOp (OpNeg, EVar "myvar"))));
    "Construct BinOp_L "
    >:: assert_equal
          (change_ast
             (Eif_L (Cursor (EVar "myvar"), EInt 2, EInt 3))
             (Construct (BinOp_L Neq)))
          (Eif_L (Cursor (EBinOp (EVar "myvar", Neq, EHole)), EInt 2, EInt 3));
    "Construct BinOp_R "
    >:: assert_equal
          (change_ast
             (Eif_C (EBool false, Cursor (EVar "myvar1"), EInt 3))
             (Construct (BinOp_L OpEq)))
          (Eif_C
             (EBool false, Cursor (EBinOp (EHole, OpEq, EVar "myvar1")), EInt 3));
    "Construct Let_L "
    >:: assert_equal
          (change_ast
             (Eif_R (EBool false, EInt 2, Cursor (EVar "myvar")))
             (Construct (Let_L "newvar")))
          (Eif_R
             (EBool false, EInt 2, Cursor (ELet ("newvar", EVar "myvar", EHole))));
    "Construct Let_R "
    >:: assert_equal
          (change_ast
             (EFun_R (TypBool, Cursor (EVar "myvar")))
             (Construct (Let_R "newvar")))
          (EFun_R (TypBool, Cursor (ELet ("newvar", EHole, EVar "myvar"))));
    "Construct If_L "
    >:: assert_equal
          (change_ast
             (EFix_R (TypBool, Cursor (EVar "myvar")))
             (Construct If_L))
          (EFix_R (TypBool, Cursor (EIf (EVar "myvar", EHole, EHole))));
    "Construct If_C "
    >:: assert_equal
          (change_ast
             (EPair_R (EInt 0, Cursor (EVar "myvar")))
             (Construct If_C))
          (EPair_R (EInt 0, Cursor (EIf (EHole, EVar "myvar", EHole))));
    "Construct If_R "
    >:: assert_equal
          (change_ast
             (EPair_L (Cursor (EVar "myvar"), EInt 0))
             (Construct If_R))
          (EPair_L (Cursor (EIf (EHole, EHole, EVar "myvar")), EInt 0));
    "Construct Fun "
    >:: assert_equal
          (change_ast
             (EPair_L (Cursor (EVar "myvar"), EInt 0))
             (Construct (Fun "newvar")))
          (EPair_L (Cursor (EFun ("newvar", THole, EVar "myvar")), EInt 0));
    "Construct Fix "
    >:: assert_equal
          (change_ast
             (EPair_L (Cursor (EVar "myvar"), EInt 0))
             (Construct (Fix "newvar")))
          (EPair_L (Cursor (Fix ("newvar", THole, EVar "myvar")), EInt 0));
    "Construct PairL "
    >:: assert_equal
          (change_ast
             (EPair_L (Cursor (EVar "myvar"), EInt 0))
             (Construct Pair_L))
          (EPair_L (Cursor (Pair_L (EVar "myvar", THole)), EInt 0));
    "Construct Pair_R "
    >:: assert_equal
          (change_ast
             (EPair_L (Cursor (EVar "myvar"), EInt 0))
             (Construct Pair_R))
          (EPair_L (Cursor (Pair_L (THole, EVar "myvar")), EInt 0));
  ]

let test_construct_type =
  [
    "Construct Type Int "
    >:: assert_equal
          (change_ast (EFun ("newvar", Cursor TBool, EHole)) (Construct TypInt))
          (EFun ("newvar", Cursor TInt, EHole));
    "Construct Type Bool "
    >:: assert_equal
          (change_ast (EFun ("newvar", Cursor TInt, EHole)) (Construct TypInt))
          (EFun ("newvar", Cursor TBool, EHole));
    "Construct Type Arrow_L "
    >:: assert_equal
          (change_ast
             (EFun ("newvar", Cursor TBool, EHole))
             (Construct TypArrow_L))
          (EFun ("newvar", Cursor (TArrow (TBool, THole)), EHole));
    "Construct Type Arrow_L "
    >:: assert_equal
          (change_ast
             (EFun ("newvar", Cursor TInt, EHole))
             (Construct TypArrow_R))
          (EFun ("newvar", Cursor (TArrow (THole, TInt)), EHole));
    "Construct Type Arrow_L "
    >:: assert_equal
          (change_ast
             (EFix ("newvar", Cursor TBool, EHole))
             (Construct TypProd_L))
          (EFix ("newvar", Cursor (TProd (TBool, THole)), EHole));
    "Construct Type Arrow_L "
    >:: assert_equal
          (change_ast
             (EFix ("newvar", Cursor TInt, EHole))
             (Construct TypProd_R))
          (EFix ("newvar", Cursor (TProd (THole, TInt)), EHole));
    "Construct Type List "
    >:: assert_equal
          (change_ast
             (EFun ("newvar", Cursor TBool, EVar "hey"))
             (Construct TypList))
          (EFun ("newvar", Cursor (TList TBool), EVar "hey"));
    "Construct Type List "
    >:: assert_equal
          (change_ast
             (EFun ("newvar", Cursor TBool, EVar "hey"))
             (Construct TypHole))
          (EFun ("newvar", Cursor THole), EVar "hey");
  ]

(* Run *)
let tests =
  "Testing run_unit_tests"
  >::: [
         "Empty Code" >:: test_empty_code;
         "Empty Tests" >:: test_empty_tests;
         "Invalid Code" >:: test_invalid_code;
         "Pass All Tests" >:: test_pass_all;
         "Pass Part of the Tests" >:: test_pass_partial;
         "Fail All Tests" >:: test_fail_all;
       ]
       @ test_move_parent @ test_move_child @ test_construct_expr
       @ test_construct_type
