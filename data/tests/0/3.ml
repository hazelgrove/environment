let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (-2 :: -1 :: []) (f (0 :: 1 :: []))) && (equal (-3 :: -5 :: []) (f (-1 :: -3 :: []))))
