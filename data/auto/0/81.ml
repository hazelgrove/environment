let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-2 :: 1 :: []) (f (-2 :: 1 :: []))) && (equal (1 :: 0 :: []) (f (1 :: 0 :: []))))
