let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-1 :: 2 :: []) (f (-4 :: -1 :: []))) && (equal (3 :: 6 :: []) (f (0 :: 3 :: []))))
