let f (x1 : int list) =
    map (fun x2 -> x2 + ?) x1
in
assert ((equal (4 :: 2 :: []) (f (1 :: -1 :: []))) && (equal (3 :: 5 :: []) (f (-2 :: 0 :: []))))
