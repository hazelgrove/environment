let f (x1 : int list) =
    filter (fun x2 -> x2 != ?) x1
in
assert ((equal (0 :: []) (f (0 :: 2 :: []))) && (equal [] (f (2 :: 2 :: []))) && (equal (-2 :: 2 :: []) (f (-1 :: 4 :: []))))
