let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (-6 :: 2 :: []) (f (-3 :: 1 :: []))) && (equal (2 :: -2 :: []) (f (1 :: -1 :: []))))
