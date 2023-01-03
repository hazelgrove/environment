let f (x1 : int list) =
    map (fun x2 -> x2 - ?) x1
in
assert ((equal (-2 :: 2 :: []) (f (0 :: 4 :: []))) && (equal (1 :: -1 :: []) (f (3 :: 1 :: []))))
