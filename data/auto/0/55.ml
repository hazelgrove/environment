let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (-4 :: -2 :: []) (f (-3 :: -1 :: []))) && (equal (-5 :: -3 :: []) (f (-4 :: -2 :: []))))
