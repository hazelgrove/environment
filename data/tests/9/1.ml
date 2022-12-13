let f x1 = 
    match x1 with
    | [] -> 2
    | x2 :: x3 -> -1
in
assert ((f [] = 2) && (f (1 :: []) = -1) && (f (1 :: 2 :: []) = -1))
