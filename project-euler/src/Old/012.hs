-- Project Euler problem 12
-- Find the first triangle number with more than n divisors, where n = 500

-- A triangle number is a number t(k) = 1 + 2 + ... + k = k*(k+1)/2
--
-- There are a couple of ways to determine the number of divisors of n
-- The easiest is to count how many elements, x, in [1..n] obey n `mod` x == 0
--
-- A faster way is to count from [1..sqrt(n)]. Any x that divides n
-- immediately tells you that n = x*(n/x), thus yielding two divisors.
-- For instance, if n == 34, x ==2 -> n = 2*17. Thus, count the number
-- of x in [1..sqrt(n)] such that n `mod` x == 0 and double it. Take
-- care of sqrt(n) in an intelligent way
--
-- The fastest way is to factor n into p1^e1 * p2^e2 * ... * pk^ek
-- The number of divisors is then (e1+1)*(e2+1)*...*(ek+1)
--
-- Last note: In general, sigma_k(n) = Total[Divisors[n]^k]
-- So if you want to find the sum of the divisors of n,
-- you want sigma_1(n). Here we are finding sigma_0(n)

sigma0_O0_1 n = length [ x | x <- [1..n], n `mod` x == 0 ]

sigma0_O0_2 n = sum [ 1 | x <- [1..n], n `mod` x == 0 ]

sigma0_O1 n = let s = floor $ sqrt $ fromIntegral n
              in if s^2 == n then 1 + 2*(length [ x | x <- [1..s-1], n `mod` x == 0 ])
                             else 2*(length [ x | x <- [1..s], n `mod` x == 0 ])

sigma0 = sigma0_O1

-- triangle k = k*(k+1)/2, but sigma0 is multiplicative
-- That is, sigma0(k*(k+1)/2) = sigma0(k)*sigma0((k+1)/2)
-- Since sigma0 takes integers, just put the /2 wherever
-- it won't break things
triangleSigma0 k | even k = (sigma0_O1 $ k `div` 2) * (sigma0 $ k+1 )
                 | odd k  = (sigma0_O1 $ (k+1) `div` 2) * (sigma0 k )


triangle k = k*(k+1) `div` 2

problem012 n = head [ x | x <- map triangle [1..], (sigma0 x) > n ]

-- This is nearly 50 times faster than problem012 unoptimized
problem012opt n = head [ k*(k+1) `div` 2 | k <- [1..], (triangleSigma0 k) > n ]

main = print ans where ans = problem012opt 500
