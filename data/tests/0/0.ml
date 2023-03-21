let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-4 :: -7 :: []) (f (-1 :: -4 :: []))) && (equal (-4 :: -3 :: []) (f (-1 :: 0 :: []))))
