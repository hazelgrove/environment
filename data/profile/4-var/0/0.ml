let f (x1 : bool) (x2 : bool) (x3 : bool) (x4 : bool) =
	? && x2 && x3 && x4
in
assert ((!(f false false false false)) && (!(f false false false true)) && (!(f false false true false)) && (!(f false false true true)) && (!(f false true false false)) && (!(f false true false true)) && (!(f false true true false)) && (!(f false true true true)) && (!(f true false false false)) && (!(f true false false true)) && (!(f true false true false)) && (!(f true false true false)) && (!(f true true false false)) && (!(f true  true false true)) && (!(f true  true  true false)) && (!(f true  true  true true )))
