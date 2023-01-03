let f (x1 : int list) =
    filter (fun x2 -> x2 = ?) x1
in
assert ((equal (2 :: []) (f (2 :: 1 :: []))) && (equal (2 :: 2 :: []) (f (2 :: 2 :: []))) && (equal [] (f (-2 :: 1 :: []))))
