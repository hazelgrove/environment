let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (4 :: 3 :: []) (f (1 :: 0 :: []))) && (equal (3 :: 5 :: []) (f (0 :: 2 :: []))))
