let f (x1 : int list) =
    filter (fun x2 -> x2 != 1) x1
in
assert ((equal (0 :: []) (f (0 :: 1 :: []))) && (equal [] (f (1 :: 1 :: []))) && (equal (-2 :: 2 :: []) (f (-2 :: 2 :: []))))
