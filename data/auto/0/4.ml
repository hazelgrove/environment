let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (-5 :: -1 :: []) (f (-4 :: 0 :: []))) && (equal (-3 :: -1 :: []) (f (-2 :: 0 :: []))))
