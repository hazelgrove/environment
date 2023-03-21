let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (0 :: 5 :: []) (f (-2 :: 3 :: []))) && (equal (3 :: 0 :: []) (f (1 :: -2 :: []))))
