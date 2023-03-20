let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (3 :: 6 :: []) (f (0 :: 3 :: []))) && (equal (-1 :: 0 :: []) (f (-4 :: -3 :: []))))
