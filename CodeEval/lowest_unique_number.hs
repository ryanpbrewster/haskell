-- lowest_unique_number.hs
{-
 - There is a game where each player picks a number from 1 to 9, writes it on
 - a paper and gives to a guide. A player wins if his number is the lowest
 - unique. We may have 10-20 players in our game.
 -
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename.
 -
 - You're a guide and you're given a set of numbers from players for the round
 - of game. E.g. 2 rounds of the game look this way:
 -
 - 3 3 9 1 6 5 8 1 5 3
 - 9 2 9 9 1 8 8 8 2 1 1
 - Output sample:
 -
 - Print a winner's position or 0 in case there is no winner. In the first line
 - of input sample the lowest unique number is 6. So player 5 wins.
 -
 - 5
 - 0
 -}





import System.Environment (getArgs)
import qualified Data.Map as Map
import Data.List (findIndex)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

atoi :: String -> Int
atoi = read

solveProblem txt = let inputs = [ map atoi $ words ln | ln <- lines txt ]
                       anss = map lowestUniquePosition inputs
                       outputs = map processOutput anss
                   in unlines outputs

processOutput Nothing = "0"
processOutput (Just n) = show (n+1)

lowestUniquePosition xs =
    let counts = Map.fromListWith (+) $ zip xs (repeat 1)
        uniques = Map.filter (==1) counts
    in if Map.null uniques
       then Nothing
       else let uniq = fst (Map.findMin uniques)
            in findIndex (==uniq) xs
