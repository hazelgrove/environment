let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (0 :: -3 :: []) (f (2 :: -1 :: []))) && (equal (-4 :: -1 :: []) (f (-2 :: 1 :: []))))
