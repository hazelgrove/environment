let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (1 :: -6 :: []) (f (3 :: -4 :: []))) && (equal (-2 :: -3 :: []) (f (0 :: -1 :: []))))
