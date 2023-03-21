let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (1 :: 5 :: []) (f (-3 :: 1 :: []))) && (equal (4 :: 3 :: []) (f (0 :: -1 :: []))))
