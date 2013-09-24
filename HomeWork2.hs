--Dmitry Petuhov
--MM 2013

module HomeWork2 where

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

con :: [a] -> [a] -> [a] 
con [] xs = xs
con (x:xs) ys = x:(con xs ys)

--List of Lists To List - LLTL
lltl :: [[a]] -> [a]
lltl [] = []
lltl ((x:xs):xs2) = (x:xs) ++ lltl xs2

lltl2 = foldl (++) []

take' :: Int -> [a] -> [a]
take' 0 _      = []
take' _ []     = []
take' n (x:xs) = x:take' (n-1) xs 

reverse :: [a] -> [a]
reverse ls = reverse' ls [] where
 reverse' [] l    = l
 reverse' (x:xs) l = reverse' xs (x:l)

--Fibonacci
fib :: Int -> Int
fib n = fib' n (0, 1) where 
	fib' 0 (a,b) = a
	fib' n (a,b) = fib' (n-1) (b, a+b)

--Rational numbers
redR (n, dn)
	| n == dn   = (1, 1)
	| otherwise = let g = gcd n dn in (n `div` g, dn `div` g)

addR (n1, dn1) (n2, dn2) = redR (n1*dn2 + n2*dn1, dn1*dn2)
subR (n1, dn1) (n2, dn2) = redR (n1*dn2 - n2*dn1, dn1*dn2)
multR (n1, dn1) (n2, dn2) = redR (n1 * n2, dn1 * dn2)
divR  (n1, dn1) (n2, dn2) 
	| n2 == 0   = error ("divR: Division by zero")
	| otherwise = multR (n1, dn1) (dn2, n2)

is0 (a, _) = a == 0

--Polynomial
addM [] m = [m]
addM p@((n1, d1):xs) m@(n2, d2) =
	if d2 < d1 then (n1,d1) : addM xs m 
    else if d2 > d1 then m:p
    else if is0 s then xs else s:xs where s = (n1+n2, d1)
--if s then xs ...
--where s = fst addR (n1,dn1) m

subM p (n,d) = addM p (-n,d)

mulM [] m = []
mulM p@((n1, d1):xs) m@(n2, d2) = (n1 * n2, d1 + d2):mulM xs m 

addP p [] = p 
addP p (x:xs) = addP (addM p x) xs

subP p [] = p 
subP p (x:xs) = subP (subM p x) xs

mulP [] p = []
mulP p [] = []
mulP p (x:xs) = addP (mulM p x) (mulP p xs)

divM (n1, d1) (n2, d2) = (n1 / n2, d1 - d2)