let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (0 :: 0 :: []) (f (0 :: 0 :: []))) && (equal (-3 :: -1 :: []) (f (-3 :: -1 :: []))))
