-- morse_code.hs
{-
 -  You have received a text encoded with Morse code and want to decode it.
 -  Input sample:
 -
 -  Your program should accept as its first argument a path to a filename. Input example is the following:
 -
 -  .- ...- ..--- .-- .... .. . -.-. -..-  ....- .....
 -  -... .... ...--
 -
 -  Each letter is separated by space char, each word is separated by 2 space chars.
 -  Output sample:
 -
 -  Print out decoded words. E.g.
 -
 -  AV2WHIECX 45
 -  BH3
 -
 -  You program has to support letters and digits only.
 -}

import System.Environment (getArgs)
import Data.List (isPrefixOf)
import qualified Data.Map as M

splitBy _ [] = []
splitBy x s = let (f,r) = slurpUntil x s
              in (f: splitBy x r)

slurpUntil _ [] = ([], [])
slurpUntil x s | x `isPrefixOf` s = ([], drop (length x) s)
               | otherwise = let (f,r) = slurpUntil x (tail s)
                             in (head s : f, r)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let lns = lines txt
                       inps = [ map words $ splitBy "  " ln | ln <- lns ]
                       outs = map fromMorse inps
                   in unlines outs

fromMorse wrds = let wrds' = [ map (morseDict M.!) wrd | wrd <- wrds ]
                 in unwords wrds'

morseDict = M.fromList [(".-"   , 'A')
                       ,("-..." , 'B')
                       ,("-.-." , 'C')
                       ,("-.."  , 'D')
                       ,("."    , 'E')
                       ,("..-." , 'F')
                       ,("--."  , 'G')
                       ,("...." , 'H')
                       ,(".."   , 'I')
                       ,(".---" , 'J')
                       ,("-.-"  , 'K')
                       ,(".-.." , 'L')
                       ,("--"   , 'M')
                       ,("-."   , 'N')
                       ,("---"  , 'O')
                       ,(".--." , 'P')
                       ,("--.-" , 'Q')
                       ,(".-."  , 'R')
                       ,("..."  , 'S')
                       ,("-"    , 'T')
                       ,("..-"  , 'U')
                       ,("...-" , 'V')
                       ,(".--"  , 'W')
                       ,("-..-" , 'X')
                       ,("-.--" , 'Y')
                       ,("--.." , 'Z')
                       ,("-----", '0')
                       ,(".----", '1')
                       ,("..---", '2')
                       ,("...--", '3')
                       ,("....-", '4')
                       ,(".....", '5')
                       ,("-....", '6')
                       ,("--...", '7')
                       ,("---..", '8')
                       ,("----.", '9')
                      ]
