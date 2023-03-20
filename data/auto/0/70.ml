let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (4 :: 2 :: []) (f (2 :: 0 :: []))) && (equal (-1 :: 2 :: []) (f (-3 :: 0 :: []))))
