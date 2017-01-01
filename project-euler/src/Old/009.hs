-- Project Euler problem 009
-- Find the Pythagorean triple (a,b,c) such that a+b+c == n, where n = 1000


problem009 n = [ (a,b,c) | a <- [1..n], b <- [a..n], let c = n-a-b, a^2+b^2==c^2 ]

main = print ans where ans = problem009 1000

triples :: Integer -> [(Integer,Integer,Integer)]
triples n = [ (a,b,c) | p <- [1..u], q <- [1..p-1], k <- [1..n `div` p],
                        let a = k*(p^2-q^2),
                        let b = k*(2*p*q),
                        let c = k*(p^2+q^2),
                        a+b+c == n ]
            where u = floor $ sqrt $ fromIntegral n

problem009opt n = head $ triples n
