let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-5 :: -8 :: []) (f (-1 :: -4 :: []))) && (equal (-3 :: -7 :: []) (f (1 :: -3 :: []))))
