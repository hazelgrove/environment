open OUnit2

let _ =
  print_string "Testing astlib.ml...";
  run_test_tt_main Test_astlib.tests;
  print_endline "";

  (* print_string "Testing astutil.ml...";
  run_test_tt_main Test_astutil.unit_tests;
  print_endline "" *)
