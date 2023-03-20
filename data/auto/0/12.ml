let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (0 :: 2 :: []) (f (-3 :: -1 :: []))) && (equal (6 :: 4 :: []) (f (3 :: 1 :: []))))
