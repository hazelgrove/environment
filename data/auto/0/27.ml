let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (0 :: -2 :: []) (f (1 :: -1 :: []))) && (equal (2 :: -4 :: []) (f (3 :: -3 :: []))))
