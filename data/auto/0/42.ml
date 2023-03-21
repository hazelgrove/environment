let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (5 :: 5 :: []) (f (1 :: 1 :: []))) && (equal (5 :: 6 :: []) (f (1 :: 2 :: []))))
