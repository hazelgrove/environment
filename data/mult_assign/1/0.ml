let f x1 = (2 * ((x1 * 2) - 1)) in
assert ((f 0 = -2) && (f 1 = 2) && (f 2 = (2 + 2 + 2)) && (f -1 = (-2 - 2 - 2)))
