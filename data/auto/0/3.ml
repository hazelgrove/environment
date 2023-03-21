let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (1 :: -1 :: []) (f (2 :: 0 :: []))) && (equal (0 :: -2 :: []) (f (1 :: -1 :: []))))
