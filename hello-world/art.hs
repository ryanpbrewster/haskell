genfun(0,r:rs)
	| r < 0.50	= (\(x,y) -> x)
	| otherwise	= (\(x,y) -> y)

genfun(d,r:rs)
	| r < 0.25	= (\(x,y) -> sin(x))
	| r < 0.50	= (\(x,y) -> cos(x))
	| r < 0.75 	= (\(x,y) -> (x+y)/2)
	| otherwise	= (\(x,y) -> x*y)
