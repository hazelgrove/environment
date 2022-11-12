let f x1 = 
    match x1 with
    | 0 -> 0
    | _ -> 1
in
assert (f 0 = 0)