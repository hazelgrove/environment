let f x1 = 
    match x1 with
    | 0 -> ?
    | _ -> 1
in
assert ((f 0 = 0) && (f 1 = 1) && (f 2 = 1))
