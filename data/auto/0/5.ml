let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (-4 :: 0 :: []) (f (-1 :: 3 :: []))) && (equal (-7 :: -6 :: []) (f (-4 :: -3 :: []))))
