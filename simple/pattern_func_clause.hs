main = putStrLn "Hello"

fact 1 = 1
fact n = n * fact (n-1)

factt (("fac",1)) = 1
factt (("fac",n)) = n * factt(("fac", n-1))
