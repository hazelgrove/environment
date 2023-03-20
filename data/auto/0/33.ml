let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (0 :: -3 :: []) (f (3 :: 0 :: []))) && (equal (-3 :: -7 :: []) (f (0 :: -4 :: []))))
