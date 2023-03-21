let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-2 :: -5 :: []) (f (1 :: -2 :: []))) && (equal (-5 :: -4 :: []) (f (-2 :: -1 :: []))))
