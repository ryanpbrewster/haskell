-- distinct_subsequences.hs
{-
 - Challenge Description:
 -
 - A subsequence of a given sequence S consists of S with zero or more elements
 - deleted. Formally, a sequence Z = z1z2..zk is a subsequence of
 - X = x1x2...xm, if there exists a strictly increasing sequence <i1,i2...ik>
 - of indicies of X such that for all j=1,2,...k we have Xij = Zj. e.g. Z=bcdb
 - is a subsequence of X=abcbdab with corresponding index sequence <2,3,5,7>
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. Each
 - line in this file contains two comma separated strings. The first is the
 - sequence X and the second is the subsequence Z. e.g.
 -
 - babgbag,bag
 - rabbbit,rabbit
 -
 - Output sample:
 -
 - Print out the number of distinct occurrences of Z in X as a subsequence e.g.
 -
 - 5
 - 3
 -}



import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = [ wordsBy "," ln | ln <- lines txt ]
                       anss = [ occurrences z x | [x,z] <- inputs ]
                   in unlines $ map show anss

wordsBy delims s = wordsBy' delims s
    where wordsBy' _ [] = []
          wordsBy' delims s = let (f,r) = break (`elem` delims) s
                              in f:wordsBy' delims (dropWhile (`elem` delims) r)


occurrences [] _ = 1
occurrences _ [] = 0
occurrences zs@(z:zr) (x:xr) | z == x    = occurrences zs xr + occurrences zr xr
                             | otherwise = occurrences zs xr
