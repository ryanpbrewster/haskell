module Problems.P079
  ( process
  ) where

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
import Data.Char (digitToInt, intToDigit)
import Data.Graph (buildG, topSort)
import Data.List (intersect)
import Data.Set (fromList, elems)

process :: String -> String
process txt =
  let logins = [map digitToInt ln | ln <- lines txt]
  in show $ solveProblem logins

solveProblem logins =
  let digit_set = elems $ fromList $ concat logins
      bounds = (minimum digit_set, maximum digit_set)
      edges = concat [zip (init login) (tail login) | login <- logins]
      g = buildG bounds edges
      passcode = intersect (topSort g) digit_set
  in map intToDigit passcode
