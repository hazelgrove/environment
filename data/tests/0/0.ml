let f x1 =
    map (fun x2 -> x2 + 1) x1
in
assert ((f [] = 0) && (f (1 :: 2 :: []) = (2 :: 3 :: [])) && (f 2 = 1))
