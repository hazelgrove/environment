let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (2 :: 6 :: []) (f (1 :: 3 :: []))) && (equal (0 :: -4 :: []) (f (0 :: -2 :: []))))
