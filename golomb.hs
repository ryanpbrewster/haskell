-- golomb.hs
{-
 - The Golomb's self-describing sequence {G(n)} is the only nondecreasing
 - sequence of natural numbers such that n appears exactly G(n) times in the
 - sequence. The values of G(n) for the first few n are
 -
 - n    1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  …
 - G(n) 1   2   2   3   3   4   4   4   5   5   5   6   6   6   6   …
 -
 - You are given that G(103) = 86, G(106) = 6137.
 -}

g1 = tail g'
    where g' = [0,1,2,2] ++ concat [replicate (g' !! k) k | k <- [3..] ]

-- g2 is MUCH slower than g1
g2 = [1,2] ++ g' 3 [2]
    where g' k (c:cs) = c : g' (k+1) (cs ++ replicate c k)

-- g3 is faster than g1
g3 = tail g'
    where g' = [0,1,2,2] ++ concat [replicate z k | (z,k) <- zip (drop 3 g') [3..] ]

main = print $ g3 !! (5*10^7)
