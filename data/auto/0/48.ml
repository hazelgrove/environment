let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (4 :: 8 :: []) (f (-1 :: -2 :: []))) && (equal (-4 :: -4 :: []) (f (1 :: 1 :: []))))
