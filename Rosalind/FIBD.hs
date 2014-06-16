-- FIBD.hs
{-
 - Problem
 - Figure 4. A figure illustrating the propagation of Fibonacci's rabbits if they die after three months.
 -
 - Recall the definition of the Fibonacci numbers from “Rabbits and Recurrence Relations”, which followed the recurrence relation Fn=Fn−1+Fn−2 and assumed that each pair of rabbits reaches maturity in one month and produces a single pair of offspring (one male, one female) each subsequent month.
 -
 - Our aim is to somehow modify this recurrence relation to achieve a dynamic programming solution in the case that all rabbits die out after a fixed number of months. See Figure 4 for a depiction of a rabbit tree in which rabbits live for three months (meaning that they reproduce only twice before dying).
 -
 - Given: Positive integers n≤100 and m≤20.
 -
 - Return: The total number of pairs of rabbits that will remain after the n-th month if all rabbits live for m months.
 - Sample Dataset
 -
 - 6 3
 -
 - Sample Output
 -
 - 4
 -}

{-
 - Let b[n] be the number of baby rabbits in month n
 -     a[n] be the number of adult rabbits in month n
 -     f[n] be the total number of rabbits
 -
 - Clearly b[n] = a[n-1], since all the adult rabbits will give birth
 -         a[n] = a[n-1]-b[n-m] + b[n-1], since some adult rabbits die
 -                                        (specifically the ones that were
 -                                        babies m months ago), and baby
 -                                        rabbits maure
 -}

import System.Environment (getArgs)

fibsd m = let as = replicate m 0 ++ 0 : zipWith3 f (drop m as) (drop 1 bs) (drop m bs)
              bs = replicate m 0 ++ 1 : drop m as
          in 0 : zipWith (+) (drop m as) (drop m bs)
    where f a b c = a-b+c

main = do
    args <- getArgs
    let [n,m] = map read args
    print $ (fibsd m) !! n
