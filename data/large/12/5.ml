let f (x1 : int list) =
    map (fun x2 -> x2 - ?) x1
in
assert (equal (4 :: 5 :: []) (f (3 :: 4 :: [])))
