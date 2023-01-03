let f (x1 : int list) =
    map (fun x2 -> x2 + 3) x1
in
assert ((equal (3 :: 4 :: []) (f (0 :: 1 :: []))) && (equal (2 :: 4 :: []) (f (-1 :: 1 :: []))))
