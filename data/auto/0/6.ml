let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (2 :: -4 :: []) (f (3 :: -3 :: []))) && (equal (1 :: -1 :: []) (f (2 :: 0 :: []))))
