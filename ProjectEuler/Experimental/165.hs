-- 165.hs
{-
 - A segment is uniquely defined by its two endpoints. By considering two line
 - segments in plane geometry there are three possibilities: the segments have
 - zero points, one point, or infinitely many points in common.
 -
 - Moreover when two segments have exactly one point in common it might be the
 - case that that common point is an endpoint of either one of the segments or
 - of both. If a common point of two segments is not an endpoint of either of
 - the segments it is an interior point of both segments.  We will call
 - a common point T of two segments L1 and L2 a true intersection point of L1
 - and L2 if T is the only common point of L1 and L2 and T is an interior point
 - of both segments.
 -
 - Consider the three segments L1, L2, and L3:
 -
 - L1: (27, 44) to (12, 32)
 - L2: (46, 53) to (17, 62)
 - L3: (46, 70) to (22, 40)
 -
 - It can be verified that line segments L2 and L3 have a true intersection
 - point. We note that as the one of the end points of L3: (22,40) lies on L1
 - this is not considered to be a true point of intersection. L1 and L2 have no
 - common point. So among the three line segments, we find one true
 - intersection point.
 -
 - Now let us do the same for 5000 line segments. To this end, we generate
 - 20000 numbers using the so-called "Blum Blum Shub" pseudo-random number
 - generator.
 -
 - s0 = 290797
 -
 - sn+1 = sn×sn (modulo 50515093)
 -
 - tn = sn (modulo 500)
 -
 - To create each line segment, we use four consecutive numbers tn. That is,
 - the first line segment is given by:
 -
 - (t1, t2) to (t3, t4)
 -
 - The first four numbers computed according to the above generator should be:
 - 27, 144, 12 and 232. The first segment would thus be (27,144) to (12,232).
 -
 - How many distinct true intersection points are found among the 5000 line
 - segments?
 -}


import ProjectEuler.Util (roll, sublists)
import Data.List (groupBy, sort)
import Data.Maybe

ss = 290797 : map ((`mod` 50515093) . (^2)) ss

tt = tail $ map (`mod` 500) ss

closeEnough [x1,y1] [x2,y2] = ((x2-x1)^2 + (y2-y1)^2) < 1e-5

solveProblem n = let pts = roll 2 [ fromIntegral k | k <- tt ]
                     segments = roll 2 pts
                     seg_pairs = sublists 2 $ take n segments
                     f = (\[seg1,seg2] -> intersectionPoint seg1 seg2)
                     intersections = mapMaybe f seg_pairs
                 in length $ groupBy closeEnough $ sort intersections

intersectionPoint :: (Fractional a, Ord a) => [[a]] -> [[a]] -> Maybe [a]
intersectionPoint [[x1,y1],[x2,y2]] [[x3,y3], [x4,y4]] =
    let cn  = x3*y1 - x4*y1 - x1*y3 + x4*y3 + x1*y4 - x3*y4
        dn  = x3*y1 - x2*y1 + x1*y2 - x3*y2 - x1*y3 + x2*y3
        den = x3*y1 - x4*y1 - x3*y2 + x4*y2 - x1*y3 + x2*y3 + x1*y4 - x2*y4
    in if den == 0 then Nothing else
    let c = cn / den
        d = dn / den
    in if 0 < c && c < 1 && 0 < d && d < 1
        then Just [x1+c*(x2-x1), y1+c*(y2-y1)]
        else Nothing

main = print $ solveProblem 5000
