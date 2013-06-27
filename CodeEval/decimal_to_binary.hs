-- decimal_to_binary.hs
{-
 - Given a decimal (base 10) number, print out its binary representation.
 - Input sample:
 -
 - File containing positive whole decimal numbers, one per line. e.g.
 -
 - 2
 - 10
 - 67
 -
 - Output sample:
 -
 - Print the decimal representation, one per line.
 - e.g.
 -
 - 10
 - 1010
 - 1000011
 -}


import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inputs = map read $ lines txt
                       outputs = map binaryString inputs
                   in unlines outputs

binaryString 0 = "0"
binaryString n = reverse $ binaryString' n
    where binaryString' 0 = ""
          binaryString' n | even n = '0' : binaryString' (n `quot` 2)
                          | odd n  = '1' : binaryString' (n `quot` 2)
