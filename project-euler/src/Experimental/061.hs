-- 061.hs
{-
 - Triangle, square, pentagonal, hexagonal, heptagonal, and octagonal numbers
 - are all figurate (polygonal) numbers and are generated by the following
 - formulae:
 -
 - Triangle        P3,n=n(1n+1)/2      1, 3, 6, 10, 15, ...
 - Square          P4,n=n(2n+0)/2      1, 4, 9, 16, 25, ...
 - Pentagonal      P5,n=n(3n−1)/2      1, 5, 12, 22, 35, ...
 - Hexagonal       P6,n=n(4n−2)/2      1, 6, 15, 28, 45, ...
 - Heptagonal      P7,n=n(5n−3)/2      1, 7, 18, 34, 55, ...
 - Octagonal       P8,n=n(6n−4)/2      1, 8, 21, 40, 65, ...
 - 
 - The ordered set of three 4-digit numbers: 8128, 2882, 8281, has three
 - interesting properties.
 - 
 -     * The set is cyclic, in that the last two digits of each number is the
 -     first two digits of the next number (including the last number with the
 -     first).
 -
 -     * Each polygonal type: triangle (P3,127=8128), square (P4,91=8281), and
 -     pentagonal (P5,44=2882), is represented by a different number in the
 -     set.
 -
 -     * This is the only set of 4-digit numbers with this property.
 - 
 - Find the sum of the only ordered set of six cyclic 4-digit numbers for which
 - each polygonal type: triangle, square, pentagonal, hexagonal, heptagonal,
 - and octagonal, is represented by a different number in the set.
 -}

import Data.Set (fromList, member, unions, elems)

(lo,hi) = (10^3, 10^4)
[tris, sqs, pents, hexs, hepts, octs] = 
    [ fromList $ takeWhile (<hi) $ dropWhile (<lo) $ scanl1 (+) [1,n..] | n <- [2..7] ]

data Node = Node { val :: Int, front :: Int, back :: Int } deriving (Eq, Ord)
instance Show Node where
    show (Node v f b) = show v

toNode n = let (q,r) = n `divMod` 100 in Node n q r

alls = map toNode $ elems $ unions [tris, sqs, pents, hexs, hepts, octs]
dests (Node _ _ b) = [ n | n <- alls, front n == b ]
graph = [ (v, dests v) | v <- alls ]