let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (2 :: 0 :: []) (f (-2 :: -4 :: []))) && (equal (5 :: 4 :: []) (f (1 :: 0 :: []))))
