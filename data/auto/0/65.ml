let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (-5 :: -7 :: []) (f (-2 :: -4 :: []))) && (equal (-7 :: 0 :: []) (f (-4 :: 3 :: []))))
