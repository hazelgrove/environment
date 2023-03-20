let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (6 :: 3 :: []) (f (3 :: 0 :: []))) && (equal (6 :: 2 :: []) (f (3 :: -1 :: []))))
