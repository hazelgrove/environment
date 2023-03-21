let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (6 :: 9 :: []) (f (-2 :: -3 :: []))) && (equal (0 :: -9 :: []) (f (0 :: 3 :: []))))
