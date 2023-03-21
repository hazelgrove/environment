let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (5 :: 3 :: []) (f (2 :: 0 :: []))) && (equal (2 :: 4 :: []) (f (-1 :: 1 :: []))))
