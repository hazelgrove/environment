let f (x1 : int list) =
    filter (fun x2 -> x2 = 0) x1
in
assert ((equal (0 :: []) (f (0 :: 1 :: []))) && (equal (0 :: 0 :: []) (f (0 :: 0 :: []))) && (equal [] (f (-2 :: 2 :: []))))
