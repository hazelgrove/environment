let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (7 :: 3 :: []) (f (3 :: -1 :: []))) && (equal (4 :: 7 :: []) (f (0 :: 3 :: []))))
