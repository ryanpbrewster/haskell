module Problems.P031 (solve) where

{-
 - In England the currency is made up of pound, £, and pence, p, and there are
 - eight coins in general circulation:
 -
 - 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
 -
 - It is possible to make £2 in the following way:
 -
 - 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
 -
 - How many different ways can £2 be made using any number of coins?
 -}

{-
 - Consider the notationally simpler problem:
 - How many ways can you construct the number 10 out of the set
 -     {1,2,3,4,5}
 - Clearly by using the first 0 elements (i.e. the set {}) you can
 - make [0..10] in
 -     {0,1,2,3,4,5,6,7,8,9,10}
 -     {1,0,0,0,0,0,0,0,0,0,0}
 - ways. That is, you can only make the number 0. Now, suppose we add
 - in the next element, so we have {1}. We can now make
 -     {0,1,2,3,4,5,6,7,8,9,10}
 -     {1,1,1,1,1,1,1,1,1,1,1}
 -
 - Now suppose we add in 2.
 -     {0,1,2,3,4,5,6,7,8,9,10}
 -     {1,1,2,2,3,3,4,4,5,5,6}
 -
 -  Now 3.
 -     {0,1,2,3,4,5,6,7,8,9,10}
 -     {1,1,2,3,4,5,7,8,10,12,14}
 -}

solve :: String
solve = show solveProblem

ways [] = 1:cycle [0]  -- can only make 0
ways (x:xs) = let others = ways xs
                  (start,rest) = splitAt x others -- x doesn't matter for [0..x-1]
                  ans = start ++ zipWith (+) rest ans
              in ans

solveProblem = let w = ways [1,2,5,10,20,50,100,200]
               in w !! 200
