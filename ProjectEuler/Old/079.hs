-- 079.hs
{-
 - A common security method used for online banking is to ask the user for
 - three random characters from a passcode. For example, if the passcode was
 - 531278, they may ask for the 2nd, 3rd, and 5th characters; the expected
 - reply would be: 317.
 -
 - The text file 079.in contains fifty successful login attempts.
 -
 - Given that the three characters are always asked for in order, analyse the
 - file so as to determine the shortest possible secret passcode of unknown
 - length.
 -}

{-
 - In order to solve this, I'm going to assume that the passcode does not
 - have repeated digits.
 -}

import Data.Char (digitToInt)
import Data.Set (fromList, elems)
import Data.List (permutations)


-- isSubsequence big small
isSubsequence _ [] = True
isSubsequence [] small = False
isSubsequence (b:bs) (s:ss) | b == s    = isSubsequence bs ss
                            | otherwise = isSubsequence bs (s:ss)

solveProblem logins =
    let digit_set = elems $ fromList $ concat logins
        passcodes = permutations digit_set
        legit pass = all (isSubsequence pass) logins
    in filter legit passcodes

main = do
    txt <- readFile "079.in"
    let logins = [ map digitToInt ln | ln <- lines txt ]
    print $ solveProblem logins
