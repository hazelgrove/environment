let f (x1 : bool) (x2 : bool) (x3 : bool) =
	( ( ( x2 && x3 ) || ( x1 && (!(x2)) ) ) || ( (!(x1)) && (!(x3)) ) )
in
assert ((f false false false) && (!(f false false true )) && (f false true  false) && (f false true  true ) && (f true  false false) && (f true  false true ) && (!(f true  true  false)) && (f true  true  true ))
