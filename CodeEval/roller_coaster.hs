-- roller_coaster.hs
{-
 - Take text and put it into "roller-coaster" case (alternating upper and
 - lowercase for each alphabetic character)
 -}

import System.Environment (getArgs)
import Data.Char (isAlpha, toUpper, toLower)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let lns = lines txt
                       anss = map rollerCoasterCase lns
                   in unlines anss

rollerCoasterCase str = rcc str True
    where rcc "" _        = ""
          rcc (x:xs) up | not (isAlpha x) = x           : rcc xs up
                        | up              = (toUpper x) : rcc xs (not up)
                        | not up          = (toLower x) : rcc xs (not up)
