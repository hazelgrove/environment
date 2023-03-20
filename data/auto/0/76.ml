let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (5 :: 1 :: []) (f (1 :: -3 :: []))) && (equal (6 :: 2 :: []) (f (2 :: -2 :: []))))
