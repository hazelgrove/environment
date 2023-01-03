let f (x1 : int list) =
    filter (fun x2 -> x2 = ?) x1
in
assert ((equal (3 :: []) (f (3 :: 1 :: []))) && (equal (3 :: 3 :: []) (f (3 :: 3 :: []))) && (equal [] (f (-2 :: 1 :: []))))
