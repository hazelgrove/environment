let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (0 :: 0 :: []) (f (1 :: -1 :: []))) && (equal (0 :: 0 :: []) (f (2 :: -3 :: []))))
