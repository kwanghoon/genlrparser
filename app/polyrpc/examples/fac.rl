
fac : Int -client-> Int
    = \n : Int @ client .
        if n <= 0 then 0
	else n * (fac (n-1))
