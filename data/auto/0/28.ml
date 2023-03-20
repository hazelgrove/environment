let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (3 :: -2 :: []) (f (2 :: -3 :: []))) && (equal (-3 :: 0 :: []) (f (-4 :: -1 :: []))))
