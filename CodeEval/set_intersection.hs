-- set_intersection.hs
{-
 - Challenge Description:
 -
 - You are given two sorted list of numbers (ascending order). The lists
 - themselves are comma delimited and the two lists are semicolon delimited.
 - Print out the intersection of these two sets.
 - Input sample:
 -
 - File containing two lists of ascending order sorted integers, comma
 - delimited, one per line. E.g.
 -
 - 1,2,3,4;4,5,6
 - 20,21,22;45,46,47
 - 7,8,9;8,9,10,11,12
 -
 - Output sample:
 -
 - Print out the ascending order sorted intersection of the two lists, one per
 - line. Print empty new line in case the lists have no intersection. E.g.
 -
 - 4
 -
 - 8,9
 -}




import System.Environment (getArgs)
import Data.List (intercalate, sort, intersect)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let lns = lines txt
                       inps = map parseInput lns
                       anss = map setIntersection inps
                       outs = map printOutputs anss
                   in unlines outs

parseInput :: String -> ([Int], [Int])
parseInput ln = let [a,b] = wordsBy (==';') ln
                    a' = map read $ wordsBy (==',') a
                    b' = map read $ wordsBy (==',') b
                in (a',b')

printOutputs lst = intercalate "," $ map show lst

-- Data.List defines `intersect` already
setIntersection (a,b) = a `intersect` b


-- alternately
setIntersection' (a,b) = mergeIntersection (sort a) (sort b)
    where mergeIntersection [] b = []
          mergeIntersection a [] = []
          mergeIntersection (a:as) (b:bs)
              | a == b = a:mergeIntersection as bs
              | a < b  = mergeIntersection as (b:bs)
              | b < a  = mergeIntersection (a:as) bs
