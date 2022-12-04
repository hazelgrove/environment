let f x1 =
    let x2 = ? in
    ? / (-(if ? then (match x1 with | 0 -> 0 | _ -> 1) else ?))
in
assert ((f 0 = 0) && (f 1 = 1) && (f 2 = 1))