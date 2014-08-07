-- LEXF.hs
{-
 - Given an alphabet, A = {a[1], ..., a[m]} and a length, k,
 - print all length-k strings that can be made from A, in order.
 -}


import System.Environment (getArgs)
import Control.Monad

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStrLn $ solveProblem txt

solveProblem txt = let (alphabet, k) = parseInput txt
                   --in unlines $ kStrings k alphabet
                   in unlines $ replicateM k alphabet

parseInput txt = let [ alphabet_str, k_str ] = lines txt
                     alphabet = concat $ words alphabet_str
                     k = read k_str
                 in (alphabet, k)

-- apparently this is an example of monads in action and if I understood them
-- better then I would have immediately thought to use
--     replicateM k alphabet
kStrings :: Int -> String -> [String]
kStrings 0 _ = [""]
kStrings k set = let strs = kStrings (k-1) set in [ x:str | x <- set, str <- strs ]
