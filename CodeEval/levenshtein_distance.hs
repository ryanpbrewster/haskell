-- levenshtein_distance.hs
{-
 - Two words are friends if they have a Levenshtein distance of 1 (For details
 - see http://en.wikipedia.org/wiki/Levenshtein_distance). That is, you can
 - add, remove, or substitute exactly one letter in word X to create word Y.
 - A word’s social network consists of all of its friends, plus all of their
 - friends, and all of their friends’ friends, and so on. Write a program to
 - tell us how big the social network for the word 'hello' is.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename.The
 - input file contains the word list. This list is also available as
 - levenshtein_distance_dictionary.in
 -
 - Output sample:
 -
 - Print out how big the social network for the word 'hello' is. e.g. The
 - social network for the word 'abcde' is 4846.
 -}

{-
 - We will just brute-force explore the reachable space, starting with a particular
 - word, using DFS.
 -
 - exploreSpace does the DFS
 - branchFromString finds all the strings at a Levenshtein distance of 1
 -}


import System.Environment (getArgs)
import qualified Data.Set as Set

alphabet = ['a'..'z']

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let dict = Set.fromList (lines txt)
                       ans = findNetworkSize "hello" dict
                   in show ans

findNetworkSize s dict = Set.size $ exploreSpace (branchFromString s) Set.empty dict

exploreSpace [] vis _ = vis
exploreSpace (s:ss) vis dict | not (Set.member s dict) = exploreSpace ss vis dict
                             | Set.member s vis        = exploreSpace ss vis dict
                             | otherwise = let vis' = Set.insert s vis
                                               opts = branchFromString s
                                           in exploreSpace (opts ++ ss) vis' dict

branchFromString s = (dropOne s) ++ (insertOne s) ++ (changeOne s)

dropOne [] = []
dropOne (x:xs) = xs : [ x:x' | x' <- dropOne xs ]

insertOne [] = [ [ch] | ch <- alphabet ]
insertOne xs'@(x:xs) = [ ch:xs' | ch <- alphabet ] ++ [ x:x' | x' <- insertOne xs ]

changeOne [] = []
changeOne (x:xs) = [ ch:xs | ch <- alphabet ] ++ [ x:x' | x' <- changeOne xs ]
