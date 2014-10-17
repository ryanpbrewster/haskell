-- delta_time.hs
{-
 - You are given the pairs of time values. The values are in the HH:MM:SS format with leading zeros. Your task is to find out the time difference between the pairs.
 - Input sample:
 - The first argument is a file that contains lines with the time pairs.
 -
 -     14:01:57 12:47:11
 -     13:09:42 22:16:15
 -     08:08:06 08:38:28
 -     23:35:07 02:49:59
 -     14:31:45 14:46:56
 -
 - Output sample:
 - Print to stdout the time difference for each pair, one per line. You must format the time values in HH:MM:SS with leading zeros.
 - For example:
 -     01:14:46
 -     09:06:33
 -     00:30:22
 -     20:45:08
 -     00:15:11
 -}

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Text.Printf

showTime t = let (h,m,s) = getHMS t in printf "%02d:%02d:%02d" h m s

getHMS :: Integer -> (Integer, Integer, Integer)
getHMS hms = let (hm, s) = hms `quotRem` 60
                 (h, m)  = hm `quotRem` 60
             in (h,m,s)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt =
    let inps = [ map parseTime $ words ln | ln <- lines txt ]
        anss = [ abs (t1 - t2) | [t1,t2] <- inps ]
    in unlines $ map showTime anss

parseTime inp = case parse pTime "" inp of
    Right v -> v
    Left _ -> error $ "Could not parse " ++ show inp

pTime = do
    [h,m,s] <- sepBy pInt (char ':')
    return $ 3600*h + 60*m + s

pInt = do
    ds <- many digit
    return $ read ds
