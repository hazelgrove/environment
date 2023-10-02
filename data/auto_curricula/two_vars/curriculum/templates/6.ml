let f (x1 : bool) (x2 : bool) =
	(!(x2))
in
assert ((f false false) && (!(f false true )) && (f true  false) && (!(f true  true )))

