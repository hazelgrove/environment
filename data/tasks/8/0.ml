let f x1 x2 = 
    if x1 = x2 then x1 + 1 else x2 - 1
in
assert ((f 0 0 = 1) && (f 0 1 = 0) && (f 1 0 = -1))
