let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-1 :: -3 :: []) (f (-1 :: -3 :: []))) && (equal (-4 :: 3 :: []) (f (-4 :: 3 :: []))))
