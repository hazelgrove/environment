let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (-2 :: -6 :: []) (f (1 :: 3 :: []))) && (equal (6 :: 6 :: []) (f (-3 :: -3 :: []))))
