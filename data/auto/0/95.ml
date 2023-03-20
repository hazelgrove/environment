let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (-4 :: 0 :: []) (f (-4 :: 0 :: []))) && (equal (-4 :: 0 :: []) (f (-4 :: 0 :: []))))
