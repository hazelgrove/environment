let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-7 :: -6 :: []) (f (-4 :: -3 :: []))) && (equal (-1 :: -5 :: []) (f (2 :: -2 :: []))))
