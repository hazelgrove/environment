let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (6 :: 6 :: []) (f (-2 :: -2 :: []))) && (equal (-6 :: 0 :: []) (f (2 :: 0 :: []))))
