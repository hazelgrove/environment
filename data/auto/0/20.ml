let f (x1 : int list) =
	map (fun x2 -> x2 - ?) x1
in
assert ((equal (6 :: 3 :: []) (f (2 :: -1 :: []))) && (equal (2 :: 5 :: []) (f (-2 :: 1 :: []))))
