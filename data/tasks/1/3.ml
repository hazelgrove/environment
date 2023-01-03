let f (x1 : int list) =
    filter (fun x2 -> x2 != ?) x1
in
assert ((equal (0 :: []) (f (0 :: 3 :: []))) && (equal [] (f (3 :: 3 :: []))) && (equal (-2 :: 2 :: []) (f (-2 :: 2 :: []))))
