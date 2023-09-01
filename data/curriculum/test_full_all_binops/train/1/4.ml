let f ( x1 : bool ) ( x2 : bool )= 
	true
in 
assert ((f false false) && (f false true) && (f true false) && (f true true))
