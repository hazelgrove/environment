let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (8 :: -2 :: []) (f (-4 :: 1 :: []))) && (equal (2 :: 4 :: []) (f (-1 :: -2 :: []))))
