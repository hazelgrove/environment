let f ( x1 : bool ) ( x2 : bool )= 
	(x1 && x2) || (! x1)
in 
assert (((f false false)) && (!(f false true)) && (!(f true false)) && ((f true true)) )