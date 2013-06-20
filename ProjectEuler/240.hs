-- 240.hs
{-
 - There are 1111 ways in which five 6-sided dice (sides numbered 1 to 6) can
 - be rolled so that the top three sum to 15. Some examples are:
 -
 - D1,D2,D3,D4,D5 = 4,3,6,3,5
 - D1,D2,D3,D4,D5 = 4,3,3,5,6
 - D1,D2,D3,D4,D5 = 3,3,3,6,6
 - D1,D2,D3,D4,D5 = 6,6,3,3,3
 -
 - In how many ways can twenty 12-sided dice (sides numbered 1 to 12) be rolled
 - so that the top ten sum to 70?
 -}

{-
 - There are <n> dice. You take the top <k> rolls, which must sum to <t>.
 - The dice each go from 1 to <d>.
 -}
import Data.List (sort, group)
import ProjectEuler.Util (tuples, ordTuples)


-- find all the partitions of n into exactly k elements of xs (repeats allowed)
-- xs must be all positive integers
partitions n k xs = partitions' n k (reverse $ sort xs)

partitions' 0 0 xs = [[]]
partitions' n 0 xs = []
partitions' n k [] = []
partitions' n k (x:xs) | k*x < n = []
                       | otherwise = let with = partitions' (n-x) (k-1) (x:xs)
                                         without = partitions' n k xs
                                     in (map (x:) with) ++ without

bruteforce n k t d = let rolls = map (reverse.sort) $ tuples n [1..d]
                         goods = filter (\r -> (sum $ take k r) == t) rolls
                     in length goods

fact 0 = 1
fact n = n * fact (n-1)

-- I refer to "counts", because they ignore order.
-- For instance, given a count of [3,3,2,1], the actual roll could be
--     [1,2,3,3], [1,3,2,3], [1,3,3,2], [2,1,3,3], [2,3,1,3], [2,3,3,1], etc.
solveProblem n k t d = let good_tops = partitions t k [1..d] -- find the good top dice rolls
                           -- then fill in the rest of the dice for those good rolls
                           all_counts = [ g ++ r | g <- good_tops
                                                 , let m = minimum g
                                                 , r <- ordTuples (>=) (n-k) [1..m] ]
                           fn = fact n
                           nrolls = [ product $ map (fact.length) $ group r | r <- all_counts ]
                       in sum $ map (fn `quot`) nrolls


main = print $ solveProblem 20 10 70 12
