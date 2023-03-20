let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-3 :: 0 :: []) (f (-3 :: 0 :: []))) && (equal (-1 :: -1 :: []) (f (-1 :: -1 :: []))))
