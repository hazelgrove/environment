let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (3 :: 9 :: []) (f (-1 :: -3 :: []))) && (equal (-3 :: 9 :: []) (f (1 :: -3 :: []))))
