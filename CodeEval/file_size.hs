-- file_size.hs
{-
 - Print the size of a file in bytes.
 - Input sample:
 -
 - Path to a filename. e.g.
 -
 - foo.txt
 -
 - Output sample:
 -
 - Print the size of the file in bytes.
 - e.g.
 -
 - 55
 -}

import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    print $ length txt
