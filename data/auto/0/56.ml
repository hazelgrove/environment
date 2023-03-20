let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (0 :: 0 :: []) (f (-3 :: -3 :: []))) && (equal (-1 :: 0 :: []) (f (-4 :: -3 :: []))))
