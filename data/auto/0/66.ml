let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (9 :: 6 :: []) (f (3 :: 2 :: []))) && (equal (-6 :: 3 :: []) (f (-2 :: 1 :: []))))
