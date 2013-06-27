-- remove_characters.hs
{-
 - Write a program to remove specific characters from a string.
 - Input sample:
 -
 - The first argument will be a text file containing an input string followed by a comma and then the characters that need to be scrubbed. e.g.
 -
 - how are you, abc
 - hello world, def
 -
 - Output sample:
 -
 - Print to stdout, the scrubbed strings, one per line. Trim out any leading/trailing whitespaces if they occur.
 - e.g.
 -
 - how re you
 - hllo worl
 -}


import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

trim s = dropWhile (==' ') s
scrub xs s = filter (not.(`elem` xs)) s

solveProblem txt = let inputs = [ (l, tail r) | ln <- lines txt
                                              , let (l,r) = break (==',') ln ]
                       outputs = [ scrub (trim r) l | (l,r) <- inputs ]
                   in unlines outputs
