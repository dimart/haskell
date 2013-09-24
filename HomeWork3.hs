--Dmitry Petuhov
--MM 2013

module HomeWork3 where

fib = 1 : 1 : [ a + b | (a,b) <- zip' fib (tail fib)] where   
				 zip' (x:xs) (y:ys) = (x,y) : zip xs ys
				 zip' []     []     = []

primes = 2 : pr [3,5..] where pr (x:xs) = x : (pr $ filter (\y -> y `mod` x /= 0) xs) 
