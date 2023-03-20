let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-4 :: -5 :: []) (f (0 :: -1 :: []))) && (equal (-1 :: -5 :: []) (f (3 :: -1 :: []))))
