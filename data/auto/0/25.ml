let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (7 :: 0 :: []) (f (3 :: -4 :: []))) && (equal (7 :: 1 :: []) (f (3 :: -3 :: []))))
