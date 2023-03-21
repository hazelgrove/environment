let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (-1 :: 0 :: []) (f (1 :: 2 :: []))) && (equal (1 :: -1 :: []) (f (3 :: 1 :: []))))
