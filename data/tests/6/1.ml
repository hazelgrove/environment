let f x1 = 
    match x1 with
    | 0 -> 1
    | _ -> 0
in
assert ((f 0 = 1) && (f 1 = 0) && (f 2 = 0))
