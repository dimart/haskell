-- Древовидно-рекурсивный процесс
fib1 :: Integer -> Integer
fib1 n 
    | n == 0    = 0
    | n == 1    = 1
    | otherwise = fib1(n-1) + fib1(n-2)

-- Линейная итерация
fib2 :: Integer -> Integer
fib2 n = fib' 0 1 n
    where 
        fib' :: Integer -> Integer -> Integer -> Integer
	fib' cur nxt n
	    | n == 0    = cur
	    | otherwise = fib' nxt (cur+nxt) (n-1) 

-- O(log(n))
fib3 :: Integer -> Integer 
fib3 n = fib' 1 0 0 1 n 
	where 
	     fib' a b p q count 
		| count == 0         = b 
		| count `mod` 2 == 0 = fib' a 
					    b 
					    ( (square p) + (square q) ) 
					    ( (square q) + (2*p*q)) 
					    (count `div` 2)
	 	| otherwise          = fib' ((b * q) + (a * q) + (a * p)) 
	 				    ((b * p) + (a * q)) 
	 				    p 
	 				    q 
	 				    (count - 1)
		where 
	            square x = x * x 
				
-- Линейно рекурсивный процесс
expt1 :: Integer -> Integer -> Integer
expt1 b n 
    | n == 0    = 1
    | otherwise = b * expt1 b (n-1)

-- Линейную итерация
expt2 :: Integer -> Integer -> Integer
expt2 b n = expt' b n 1 
    where 
        expt' b n product
	    | n == 0       = product
	    | otherwise    = expt' b (n-1) (b * product)

-- O(log(n))
expt3 :: Integer -> Integer -> Integer 
expt3 b n
    | n == 0          = 1
    | n `mod` 2 == 0  = square $ expt3 b (n `div` 2)
    | otherwise       = b * (expt3 b (n-1))
    where 
        square x = x * x 
