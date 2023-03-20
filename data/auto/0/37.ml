let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (2 :: -4 :: []) (f (2 :: -4 :: []))) && (equal (0 :: -3 :: []) (f (0 :: -3 :: []))))
