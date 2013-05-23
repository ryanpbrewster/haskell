-- optimization_test_1.hs

{- Lessons learned:
 - test4 << test5 << test1 < test2, test3
 - 
 - The timings for (ntests, t) == (5000, 5000):
 -     sum1: 1.78
 -     sum2: 3.58
 -     sum3: 3.59
 -     sum4: 0.015
 -     sum5: 0.17
 -
 - Trying to use arrays and being dumb about it is
 - worse than just being dumb.
 -
 - Using arrays intelligently is hugely successful
 - Using lists to remember results is kind of helpful.
 - You should really just use arrays, though.
 -}

import Data.Array

ntests = 5000
t = 5000

perm_memo = listArray (0,t) $ foldList1 (+) [0..t]
perm_list = foldList1 (+) [0..t]

sum1 n = sum [0..n]
sum2 n = memo ! n
    where memo = listArray (0,t) $ foldList1 (+) [0..t]
sum3 n = let memo = listArray (0,t) $ foldList1 (+) [0..t]
         in memo ! n
sum4 n = perm_memo ! n
sum5 n = perm_list !! n

foldList1 f (x:xs) = foldList f x xs

foldList _ init [] = [init]
foldList f init (x:xs) = init:(foldList f (f init x) xs)


test1 = print $ maximum [ sum1 t | i <- [1..ntests] ]
test2 = print $ maximum [ sum2 t | i <- [1..ntests] ]
test3 = print $ maximum [ sum3 t | i <- [1..ntests] ]
test4 = print $ maximum [ sum4 t | i <- [1..ntests] ]
test5 = print $ maximum [ sum5 t | i <- [1..ntests] ]

-- main = testslow
main = test5
