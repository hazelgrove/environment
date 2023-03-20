let f (x1 : int list) =
	map (fun x2 -> x2 + ?) x1
in
assert ((equal (-2 :: 0 :: []) (f (-2 :: 0 :: []))) && (equal (-3 :: 1 :: []) (f (-3 :: 1 :: []))))
