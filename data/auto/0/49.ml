let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (9 :: 0 :: []) (f (3 :: 0 :: []))) && (equal (-6 :: -9 :: []) (f (-2 :: -3 :: []))))
