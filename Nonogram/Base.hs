module Nonogram.Base
where

import qualified Data.Array as A
import Data.List (intercalate, transpose)

data Color = White | Black deriving (Eq, Ord)

instance Show Color where
  show White = " "
  show Black = "x"

data Constraint = Constraint (Int, Int) deriving (Eq) -- number of white/black squares
instance Show Constraint where
  show (Constraint (w,b)) = show b

data Constraints = Constraints { rows :: [Constraint]
                               , columns :: [Constraint]
                               } deriving (Eq, Show)

dimensions (Constraints rs cs) = (length rs, length cs)


getRows :: A.Array (Int,Int) a -> [[a]]
getRows arr =
  let ((1,1), (nr,nc)) = A.bounds arr
  in [ [arr A.! (i,j) | j <- [1..nc] ] | i <- [1..nr] ]

data Solution = Solution (A.Array (Int, Int) Color)
prettyPrint (Solution sol) =
  intercalate "\n" [ concat $ map show row | row <- getRows sol ]


count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

binCount :: [Color] -> Constraint
binCount xs = Constraint ( count (==White) xs
                         , count (==Black) xs
                         )

generateConstraints (Solution sol) =
  let rows = getRows sol
      cols = transpose rows
  in Constraints (map binCount rows) (map binCount cols)
