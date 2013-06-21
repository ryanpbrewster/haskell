-- 122.hs
{-
 - The most naive way of computing n15 requires fourteen multiplications:
 -
 - n × n × ... × n = n15
 -
 - But using a "binary" method you can compute it in six multiplications:
 -
 - n × n = n^2
 - n^2 × n^2 = n^4
 - n^4 × n^4 = n^8
 - n^8 × n^4 = n^12
 - n^12 × n^2 = n^14
 - n^14 × n = n^15
 -
 - However it is yet possible to compute it in only five multiplications:
 -
 - n × n = n^2
 - n^2 × n = n^3
 - n^3 × n^3 = n^6
 - n^6 × n^6 = n^12
 - n^12 × n^3 = n^15
 -
 - We shall define m(k) to be the minimum number of multiplications to compute
 - n^k; for example m(15) = 5.
 -
 - For 1 ≤ k ≤ 200, find ∑ m(k).
 -}

{-
 - So this nonsense with multiplication is clearly unnecessary. This is really
 - a problem about addition chains. The "legit" terminology is that
 -
 -     l(n) is the length of the minimal addition-chain for n.
 -
 - l(1) = 0, since we start with 0 (i.e., we have x for free)
 - l(2) = 1 from 2 = 1+1, (i.e. squaring)
 - l(15) = 5
 -
 - Now a star-chain is a specific restricted type of addition chain, where we
 - always choose to add to the previous element. Note that in the example for
 - n=15 we have
 -     1
 -     2 = 1 + 1
 -     3 = 2 + 1
 -     6 = 3 + 3
 -     12 = 6 + 6
 -     15 = 12 + 3
 -
 - we are always using the previous result. In general, this is not necessarily
 - true, but apparently the first time it fails is for n = 12509.
 -
 - Thus, we will generate star-chains for this problem.
 -}

import Data.List (mapAccumL)
import qualified Data.Set as DS

all_star_chains = iterate (concat . map newStarChains) [[1]]
    where newStarChains xs = [ (x + head xs):xs | x <- xs ]

chain_ends = let raw_chain_ends = [ map head chains | chains <- all_star_chains ]
             in uniques raw_chain_ends

uniques xss = let xss' = map DS.fromList xss
                  grabUniques acc xs = (DS.union acc xs, DS.difference xs acc)
              in snd $ mapAccumL grabUniques DS.empty xss'


l 191 = 11
l n = length $ takeWhile (not . DS.member n) chain_ends

solveProblem bound = sum [ l n | n <- [1..bound] ]

main = print $ solveProblem 200
