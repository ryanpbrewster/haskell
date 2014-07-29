-- PROB.hs
{-
 - Problem
 -
 - A random string is constructed so that the probability of choosing each subsequent symbol is based on a fixed underlying symbol frequency.
 -
 - GC-content offers us natural symbol frequencies for constructing random DNA
 - strings. If the GC-content is x, then we set the symbol frequencies of C and
 - G equal to x2 and the symbol frequencies of A and T equal to 1−x2. For
 - example, if the GC-content is 40%, then as we construct the string, the next
 - symbol is 'G'/'C' with probability 0.2, and the next symbol is 'A'/'T' with
 - probability 0.3.
 -
 - Given: A DNA string s of length at most 100 bp and an array A containing at most 20 numbers between 0 and 1.
 -
 - Return: An array B having the same length as A in which B[k] represents the
 - common logarithm of the probability that a random string constructed with
 - the GC-content found in A[k] will match s exactly.
 -
 - Sample Dataset
 -     ACGATACAA
 -     0.129 0.287 0.423 0.476 0.641 0.742 0.783
 -
 - Sample Output
 -     -5.737 -5.217 -5.263 -5.360 -5.958 -6.628 -7.009
 -}

import qualified Data.Map as M
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let [dna, gc_strs] = lines txt
                       gcs = map read $ words gc_strs
                       anss = [ fastf gc dna | gc <- gcs ]
                       outs = map show anss
                   in unwords outs

-- P(str) = Product[ P(str[i]) ]
-- The probability is the product of each character's individual probability
-- They are independent trials
f :: Double -> String -> Double
f gc str = product $ map (prob_map M.!) str
    where prob_map = M.fromList [('A', 0.5*(1-gc))
                                ,('T', 0.5*(1-gc))
                                ,('C', 0.5*gc)
                                ,('G', 0.5*gc)
                                ]

-- log(P(str))/log(10) = log( Product[ P(str[i]) ] )/log(10)
--                     = Sum[ log(P(str[i]))/log(10) ]
-- If we're going to be taking the log of P(str) anyways,
-- we can build it up directly from the logarithms (and potentially
-- avoid any troublesome floating point issues)
fastf :: Double -> String -> Double
fastf gc str = sum $ map (prob_map M.!) str
    where prob_map = M.fromList [('A', log (0.5*(1-gc)) / log 10)
                                ,('T', log (0.5*(1-gc)) / log 10)
                                ,('C', log (0.5*gc)     / log 10)
                                ,('G', log (0.5*gc)     / log 10)
                                ]
