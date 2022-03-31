open OUnit2

let rec test_set (tests : ('a * 'a) list) (cmp : 'a -> 'a -> bool) =
  match tests with
    | [] -> ()
    | hd :: tl -> 
      let (a, b) = hd in 
      assert_equal ~cmp:cmp a b;
      test_set tl cmp

let rec test_exception_set (tests : ((unit -> 'a) * exn) list) = 
  match tests with
    | [] -> ()
    | hd :: tl ->
      let (a, error) = hd in 
      assert_raises error a;
      test_exception_set tl
