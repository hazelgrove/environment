let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (-6 :: 9 :: []) (f (-2 :: 3 :: []))) && (equal (3 :: 3 :: []) (f (1 :: 1 :: []))))
