-- optimization_test_1.hs

{- Lessons learned:
 - pretty much anything with an array is fast, if you use optimization
 - if you do NOT use optimization, then you MUST be careful, as <let>
 - and <where> constructs can bite you hard
 - 
 - The timings for t == 10000 at -O3
 -     sum1: {0.520, 0.514, 0.510} -> 0.51
 -     sum2: {0.014, 0.013, 0.005} -> 0.01
 -     sum3: {0.010, 0.014, 0.008} -> 0.01
 -     sum4: {0.008, 0.014, 0.012} -> 0.01
 -     sum5: {0.234, 0.238, 0.232} -> 0.23
 -     sum6: {0.015, 0.014, 0.011} -> 0.01
 -
 - The timings for t == 10000 at -O0
 -     sum1: {3.711, 3.744, 3.787} -> 3.75
 -     sum2: {18.82, 18.91, 19.97} -> 18.9
 -     sum3: {18.92, 19.67, 19.10} -> 19.0
 -     sum4: {0.008, 0.022, 0.009} -> 0.01
 -     sum5: {0.204, 0.203, 0.205} -> 0.20
 -     sum6: {0.680, 0.688, 0.704} -> 0.69
 -
 - Trying to use arrays and being dumb about it is
 - worse than just being dumb.
 -
 - Using arrays intelligently is hugely successful
 - Using lists to remember results is kind of helpful.
 - You should really just use arrays, though.
 -}

import Data.Array

t = 10000

perm_memo = listArray (0,t) $ foldList1 (+) [0..t]
perm_list = foldList1 (+) [0..t]

sum1 n = sum [0..n]
sum2 n = memo ! n
    where memo = listArray (0,t) $ foldList1 (+) [0..t]
sum3 n = let memo = listArray (0,t) $ foldList1 (+) [0..t]
         in memo ! n
sum4 n = perm_memo ! n
sum5 n = perm_list !! n
sum6 n = memo ! n
    where memo = listArray (0,t) perm_list

foldList1 f (x:xs) = foldList f x xs

foldList _ init [] = [init]
foldList f init (x:xs) = init:(foldList f (f init x) xs)


test1 = print $ maximum [ sum1 i | i <- [1..t] ]
test2 = print $ maximum [ sum2 i | i <- [1..t] ]
test3 = print $ maximum [ sum3 i | i <- [1..t] ]
test4 = print $ maximum [ sum4 i | i <- [1..t] ]
test5 = print $ maximum [ sum5 i | i <- [1..t] ]
test6 = print $ maximum [ sum6 i | i <- [1..t] ]

main = test6
