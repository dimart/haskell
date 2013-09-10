--Dmitry Petuhov
--MM 2013

module HomeWork where

gcd' :: Integer -> Integer -> Integer
gcd' a b | b < 0 || a < 0 = error "gcd: Wrong arguments"
         | b == 0         = a
         | otherwise      = gcd' b (a `mod` b)

lcm' :: Integer -> Integer -> Integer
lcm' a b = (a `div` (gcd' a b) * b)

coprime :: Integer -> Integer -> Bool
coprime a b = (gcd' a b == 1)

--Euler's totient function
phi :: Integer -> Int
phi n = length [x | x <- [1..n], coprime x n]

--Tests
test = (testGCD, testLCM, testPHI) where
 testGCD = (gcd' 9 3 == 3,
	        gcd' 1071 462 == 21,
	        gcd' 2330 3770 == 10)
 testLCM = (lcm' 4 6 == 12,
	        lcm' 16 20 == 80,
	        lcm' 21 6 == 42)
 testPHI = (phi 23 == 22,
	        phi 36 == 12,
	        phi 30 == 8)

--Just For Fun>>>

isPrime :: Integer -> Bool
isPrime a | a <= 0         = error "isPrime: Non-positive argument"
          | otherwise      = prime 2 a where
 prime d a | d * d > a       = True
           | a `mod` d == 0  = False
 	       | otherwise       = prime (d+1) a

--McCarthy 91 function
m91 :: Integer -> Integer
m91 n | n > 100   = n - 10
	  | otherwise = m91(m91(n+11))
