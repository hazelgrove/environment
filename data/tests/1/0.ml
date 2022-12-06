let f (x1 : int list) =
    map (fun x2 -> x2 + 1) (filter (fun x2 -> x2 != 0) x1)
in
assert ((equal (2 :: []) (f (0 :: 1 :: []))) && (equal [] (f (0 :: 0 :: []))) && (equal (-1 :: 2 :: []) (f (-2 :: 1 :: []))))
