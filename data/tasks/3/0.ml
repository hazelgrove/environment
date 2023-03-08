let f (x1 : int list) = 
    fold (fun x2 -> fun x3 -> x2 + 1) ? x1
in
assert ((f [] = 0) && (f (1 :: []) = 1) && (f (-2 :: 2 :: []) = 2))
