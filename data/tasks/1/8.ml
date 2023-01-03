let f (x1 : int list) =
    filter (fun x2 -> x2 != ?) x1
in
assert ((equal (3 :: []) (f (3 :: 4 :: []))) && (equal (3 :: 1 :: []) (f (3 :: 1 :: []))) && (equal [] (f (4 :: 4 :: []))))
