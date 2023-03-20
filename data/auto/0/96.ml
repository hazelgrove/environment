let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-1 :: 0 :: []) (f (2 :: 3 :: []))) && (equal (0 :: -1 :: []) (f (3 :: 2 :: []))))
