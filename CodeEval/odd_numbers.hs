-- odd_numbers.hs
{-
 - Print the odd numbers from 1 to 99.
 - Input sample:
 -
 - None
 - Output sample:
 -
 - Print the odd numbers from 1 to 99, one number per line.
 -}

solveProblem a b = let outputs = [a,a+2..b]
                   in unlines $ map show outputs

main = do
    putStr $ solveProblem 1 99
