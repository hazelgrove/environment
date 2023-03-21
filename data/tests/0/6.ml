let f (x1 : int list) =
	map (fun x2 -> x2 * ?) x1
in
assert ((equal (0 :: -6 :: []) (f (0 :: 2 :: []))) && (equal (-9 :: -9 :: []) (f (3 :: 3 :: []))))
