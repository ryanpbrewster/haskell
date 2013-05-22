-- 016.hs
-- Project Euler problem 16
-- Find the sum of the base-b digits of n

integerDigits :: Integer -> Integer -> [Integer]
integerDigits n b | b <= 1    = error $ "Trying to expand in base " ++ (show b) ++ " < 2"
                  | n < b     = [n]
                  | otherwise = let (q,r) = n `divMod` b in
                                    (integerDigits q b) ++ [r]


problem016 n b = sum $ integerDigits n b

main = print ans where ans = problem016 (2^1000) 10
