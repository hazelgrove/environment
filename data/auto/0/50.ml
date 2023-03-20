let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (-3 :: 6 :: []) (f (-1 :: 2 :: []))) && (equal (-3 :: 0 :: []) (f (-1 :: 0 :: []))))
