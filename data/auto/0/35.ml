let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-3 :: -2 :: []) (f (-1 :: 0 :: []))) && (equal (-1 :: 1 :: []) (f (1 :: 3 :: []))))
