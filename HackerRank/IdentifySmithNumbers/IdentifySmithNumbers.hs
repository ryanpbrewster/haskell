-- IdentifySmithNumers.hs
-- See problem_statement.pdf

import Data.Char (digitToInt)

primes = 2 : filter isPrime [3,5..]

isComposite n = any (\p -> n `mod` p == 0) (takeWhile (\p -> p*p <= n) primes)

isPrime = not . isComposite

factor :: Int -> [Int]
factor n = factor' n primes
    where factor' n (p:ps) | n `mod` p == 0 = p : factor' (n `div` p) (p:ps)
                           | p*p > n        = if n > 1 then [n] else []
                           | otherwise      = factor' n ps

isSmith :: Int -> Bool
isSmith n =
    let factor_digits = map digitToInt $ concat $ map show $ factor n
        original_digits = map digitToInt $ show n
    in sum factor_digits == sum original_digits

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

main = do
    n <- readLn :: IO Int
    print $ boolToInt (isSmith n)
