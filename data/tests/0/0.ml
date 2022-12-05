let f x1 =
    map (fun x2 -> x2 + 1) x1
in
assert ((equal (1 :: 2 :: []) (f (0 :: 1 :: []))) && (equal (0 :: 2 :: []) (f (-1 :: 1 :: []))))
