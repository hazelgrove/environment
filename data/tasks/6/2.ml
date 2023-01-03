let f x1 = 
    match x1 with
    | 1 -> 0
    | _ -> 1
in
assert ((f 0 = 1) && (f 1 = 0) && (f 2 = 1))
