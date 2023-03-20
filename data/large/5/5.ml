let f (x1 : int list) =
    map (fun x2 -> x2 + ?) x1
in
assert (equal (3 :: 4 :: []) (f (4 :: 5 :: [])))
