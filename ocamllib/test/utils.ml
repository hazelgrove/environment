open OUnit2

let rec test_set (tests : ('a * 'a) list) (cmp : 'a -> 'a -> bool) =
  match tests with
    | [] -> ()
    | hd :: tl -> 
      let (a, b) = hd in 
      assert_equal ~cmp:cmp a b;
      test_set tl cmp
