let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (-1 :: 1 :: []) (f (-1 :: 1 :: []))) && (equal (0 :: -4 :: []) (f (0 :: -4 :: []))))
