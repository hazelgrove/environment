let f ( x1 : bool ) ( x2 : bool ) ( x3 : bool )= 
	?
in 
assert ((f 0 0 0) = 0 && ((f 0 0 1) = 1 && ((f 0 1 0) = 0 && ((f 0 1 1) = 0 && ((f 1 0 0) = 0 && ((f 1 0 1) = 0 && ((f 1 1 0) = 1 && (f 1 1 1) = 0)))))))
