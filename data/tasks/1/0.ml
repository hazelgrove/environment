let f (x1 : int list) =
    filter (fun x2 -> x2 != ?) x1
in
assert ((equal (1 :: []) (f (0 :: 1 :: []))) && (equal [] (f (0 :: 0 :: []))) && (equal (-2 :: 2 :: []) (f (-2 :: 2 :: []))))
