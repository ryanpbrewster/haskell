-- farey.hs
{-
 - A Farey sequence contains all reduced fractions n/d
 - where gcd(n,d) == 1.
 -
 - It is generally defined by the maximum denominator d_max
 -}

{-
 - I derived the iterative formula used in `farey` in $HOME/prog/c/farey_gen.c
 - The iterative method is much faster than the recursive method.
 -}

import Data.Ratio

farey dm = let fracs = map head $ iterate nextTerm [[0,1],[1,dm]]
           in takeWhile (\[n,d] -> n <= d) fracs
    where nextTerm [[a,b], [c,d]] = let k = (b + dm) `div` d
                                    in [[c,d], [k*c-a, k*d-b]]

-- fareyCool, named because I think it's kinda neat, looks a lot like the
-- standard Haskell implementation of the Fibonacci sequence
fareyCool dm = takeWhile (\[n,d] -> n <= d) $ fareyCool' dm
fareyCool' dm = let fracs = [0,1]:[1,dm]:zipWith nextTerm fracs (tail fracs)
                in fracs
    where nextTerm [a,b] [c,d] = let k = (b + dm) `div` d
                                 in [k*c-a, k*d-b]


farey' dm = [[0,1]] ++ (farey_h dm [0,1] [1,1]) ++ [[1,1]]
farey_h dm [a,b] [e,f] =
    let c' = a + e
        d' = b + f
        g = gcd c' d'
        c = c' `div` g
        d = d' `div` g
        in if d > dm then []
           else (farey_h dm [a,b] [c,d]) ++ [[c,d]] ++ (farey_h dm [c,d] [e,f])

farey'' dm = [0%1] ++ (farey_h' dm (0%1) (1%1)) ++ [1%1]
farey_h' dm lo hi =
    let c = (numerator lo)   + (numerator hi)
        d = (denominator lo) + (denominator hi)
        mid = c % d
    in if (denominator mid) > dm then []
       else (farey_h' dm lo mid) ++ [mid] ++ (farey_h' dm mid hi)


main = print $ length $ fareyCool 1000
