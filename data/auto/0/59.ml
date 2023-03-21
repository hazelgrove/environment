let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (3 :: 3 :: []) (f (0 :: 0 :: []))) && (equal (4 :: 0 :: []) (f (1 :: -3 :: []))))
