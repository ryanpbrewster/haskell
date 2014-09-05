-- nice_angles.hs
{-
 - Convert a decimal expression for an angle into minutes/seconds format
 -}

import Text.Printf
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

solveProblem txt = let inps = map read $ lines txt
                       anss = map toAngle inps
                   in unlines $ map show anss









data Angle = Angle { getDegrees :: Int
                   , getMinutes :: Int
                   , getSeconds :: Int
                   } deriving (Eq, Ord)

instance Show Angle where
    show (Angle d m s) = printf "%d.%02d'%02d\"" d m s

toAngle :: Double -> Angle
toAngle x = let d  = floor x
                r  = 60 * (x - fromIntegral d)
                m  = floor r
                r' = 60 * (r - fromIntegral m)
                s = floor r'
            in Angle d m s
