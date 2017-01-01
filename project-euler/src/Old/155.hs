-- 155.hs
{-
 - An electric circuit uses exclusively identical capacitors of the same value
 - C.  The capacitors can be connected in series or in parallel to form
 - sub-units, which can then be connected in series or in parallel with other
 - capacitors or other sub-units to form larger sub-units, and so on up to
 - a final circuit.
 -
 - Using this simple procedure and up to n identical capacitors, we can make
 - circuits having a range of different total capacitances. For example, using
 - up to n=3 capacitors of 60 F each, we can obtain the following 7 distinct
 - total capacitance values:
 -
 - If we denote by D(n) the number of distinct total capacitance values we can
 - obtain when using up to n equal-valued capacitors and the simple procedure
 - described above, we have: D(1)=1, D(2)=3, D(3)=7 ...
 -
 - Find D(18).
 -
 - Reminder : When connecting capacitors C1, C2 etc in parallel, the total
 - capacitance is CT = C1 + C2 +..., whereas when connecting them in series,
 - the overall capacitance is given by:
 -}

import Data.Ratio
import qualified Data.Set as DS

-- "caps n" returns a list of sets
-- let cs = caps n
-- "cs !! k" contains the capacitance values of circuits that can only be made using
-- exactly (n-k) capacitors. ((If the n-k bit bothers you, let cs = reverse $ caps n.))
-- Note: It only contains the circuits with capacitance >= 1
-- For any value c, 1/c is also possible.
--
-- It works by counting up. caps 1 is trivial to evaluate
-- From there on out, it builds up incrementally.
-- caps 2 combines (caps 1) with (caps 1)
-- caps 3 combines (caps 1) with (caps 2)
-- caps 4 combines (caps 1) with (caps 3) and (caps 2) with (caps 2)
-- etc.
caps n = caps' (n-1) [DS.fromList [1%1]]
caps' 0 cs = cs
caps' n cs =
    let l = 1 + length cs
        cs_lo = take (l `div` 2) cs
        cs_hi = take (l `div` 2) (reverse cs)
        newcaps = DS.unions $ zipWith (joinCaps) cs_lo cs_hi
        unique_newcaps = foldl DS.difference newcaps cs
    in caps' (n-1) (cs ++ [unique_newcaps])

joinCaps cs1 cs2 = let cs1' = DS.elems cs1
                       cs2' = DS.elems cs2
                       ser1 = filter (>= 1) [ c1 + c2         | c1 <- cs1', c2 <- cs2' ]
                       ser2 = filter (>= 1) [ c1 + 1/c2       | c1 <- cs1', c2 <- cs2' ]
                       ser3 = filter (>= 1) [ 1/c1 + c2       | c1 <- cs1', c2 <- cs2' ]
                       ser4 = filter (>= 1) [ 1/c1 + 1/c2     | c1 <- cs1', c2 <- cs2' ]
                       par1 = filter (>= 1) [ 1/(1/c1 + 1/c2) | c1 <- cs1', c2 <- cs2' ]
                       par2 = filter (>= 1) [ 1/(1/c1 + c2)   | c1 <- cs1', c2 <- cs2' ]
                       par3 = filter (>= 1) [ 1/(c1 + 1/c2)   | c1 <- cs1', c2 <- cs2' ]
                       par4 = filter (>= 1) [ 1/(c1 + c2)     | c1 <- cs1', c2 <- cs2' ]
                   in DS.unions $ map DS.fromList [ser1, ser2, ser3, ser4,
                                                   par1, par2, par3, par4]

solveProblem n = let ans = sum $ map DS.size $ caps n
                 in 2*ans - 1

main = print $ solveProblem 18
