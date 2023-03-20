let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (1 :: 1 :: []) (f (-1 :: -1 :: []))) && (equal (2 :: 5 :: []) (f (0 :: 3 :: []))))
