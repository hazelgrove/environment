let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (-6 :: -5 :: []) (f (-4 :: -3 :: []))) && (equal (-3 :: 0 :: []) (f (-1 :: 2 :: []))))
