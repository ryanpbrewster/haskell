-- coin_combos.hs
{- A couple solutions to the following question:
 -     Given a set C of positive integers, how many sets S are there where
 -     sum(S) == v and S contains only the elements of C (with repetitions,
 -     possibly).
 -
 - The reason I call this "coin_combos" is because the standard example is
 -     How many ways are there to make $1.00 out of pennies, nickels, dimes,
 -     and quarters?
 - This maps onto C = {1, 5, 10, 25} and v = 100.
 -}

 {-
  - There are various solutions for [1,5,10,25] and v = 1000
  -     * 4.554s --- Brute-force recursive use-it/lose-it solution
  -     * 0.097s --- iterative solution
  -     * 0.006s --- constructive solution using infinite lists
  -     * 0.007s --- constructive solution using finite lists
  -
  - For [1,3..99] and v = 1000
  -     * ?????s --- recursive
  -     * 1.707s --- iterative
  -     * 0.022s --- infinite constructive
  -     * 0.021s --- finite constructive
  -}

import Data.Array (listArray, bounds, (//), (!))
import Data.List (sort)

-- coinCombosRec coins target
coinCombosRec [] _ = 0
coinCombosRec _ t | t < 0  = 0
                  | t == 0 = 1
coinCombosRec (c:cs) targ = let useit = coinCombosRec (c:cs) (targ-c)
                                loseit = coinCombosRec cs targ
                            in useit + loseit


-- coinCombosIter coins target
-- This is haskell grossness for the following iterative code:
--     combos := make([]int, targ+1)
--     combos[0] = 1
--     for _, c := range(coins) {
--         for i := c; i <= targ; i++ {
--             combos[i] += combos[i-c]
--         }
--     }
--     return combos[targ]
coinCombosIter _ 0 = 1
coinCombosIter [] _ = 0
coinCombosIter coins targ =
    let combos = listArray (0,targ) (1:(take targ $ cycle [0]))
        final_combos = updateCombos coins targ combos
    in final_combos ! targ

updateCombos [] _ combos = combos
updateCombos (c:cs) targ combos = let combos' = (updateCombos' c c targ combos)
                                  in updateCombos cs targ combos'

updateCombos' coin cur targ combos | cur > targ = combos
updateCombos' coin cur targ combos = let val' = combos!cur + combos!(cur-coin)
                                         combos' = combos // [(cur,val')]
                                     in updateCombos' coin (cur+1) targ combos'

-- coinCombosGen coins target
coinCombosGen coins targ = (coinCombosGen' coins) !! targ
coinCombosGen' [] = 1:cycle [0]
coinCombosGen' (c:cs) = let combos = coinCombosGen' cs
                            (start,rest) = splitAt c combos
                            combos' = start ++ zipWith (+) rest combos'
                        in combos'

-- coinCombosGenFinite coins target
coinCombosGenFinite coins targ = last $ coinCombosGenFinite' coins targ
coinCombosGenFinite' [] targ = 1:(take targ $ cycle [0])
coinCombosGenFinite' (c:cs) targ =
    let combos = coinCombosGenFinite' cs targ
        (start,rest) = splitAt c combos
        combos' = start ++ zipWith (+) rest (take (targ+1-c) combos')
    in combos'

coinCombosInf coins = coinCombosInf' coins (1:cycle [0]) 0
coinCombosInf' [] combos front = drop front combos
coinCombosInf' (c:cs) combos front = let (start, rest) = splitAt c combos
                                         combos' = start ++ zipWith (+) rest combos'
                                         start' = drop front start
                                     in start' ++ (coinCombosInf' cs combos' c)

test f = print $ f [1,3..99] 1000

-- main = test coinCombosRec
-- main = test coinCombosIter
-- main = test coinCombosGen
-- main = test coinCombosGenFinite
main = test coinCombosInf
