let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (3 :: 1 :: []) (f (-1 :: -3 :: []))) && (equal (4 :: 6 :: []) (f (0 :: 2 :: []))))
