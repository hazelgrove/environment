let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (4 :: -3 :: []) (f (-4 :: 3 :: []))) && (equal (2 :: -3 :: []) (f (-2 :: 3 :: []))))
