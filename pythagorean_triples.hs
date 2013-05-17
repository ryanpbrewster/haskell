module PythagoreanTriples
( triples
, triples_opt
) where

is_pythag a b c = a*a + b*b == c*c

triples n = [(a,b,c) | a<-[1..n], b<-[a..n], c<-[b..n], is_pythag a b c]

triples_opt bound = [ (a,b,c) | m <- (takeWhile (\x -> x*x <= bound) [1..])
                              , n <- [1..m-1]
                              , gcd (m*m-n*n) (2*m*n) == 1
                              , k <- [1..bound `quot` (m*m+n*n)]
                              , let a = k*(m*m - n*n)
                              , let b = k*(2*m*n)
                              , let c = k*(m*m + n*n)
                              ]
