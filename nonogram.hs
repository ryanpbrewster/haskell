import System.Environment (getArgs)
import qualified Data.Array as A
import Data.List (intercalate, group)
import Control.Monad (replicateM)
import qualified Data.Map as M

main = do
    args <- getArgs
    let [m,n] = map read args :: [Int]
    print $ f (m,n)

constraintMap (m,n) =
    M.fromListWith (+) [(constraints b, 1) | b <- generateAllBoards (m,n) ]

f (m,n) =
    let unambiguous = M.size $ M.filter (==1) $ constraintMap (m,n)
        total       = 2^(m*n)
    in (fromIntegral unambiguous) / (fromIntegral total)

data Cell = Empty | Full deriving (Show, Eq, Ord)
data Board = Board (A.Array (Int, Int) Cell)
data Constraints = Constraints [[Int]] [[Int]] deriving (Eq, Ord)

lineConstraint :: [Cell] -> [Int]
lineConstraint xs =
    let blocks = filter (\g -> head g /= Empty) $ group xs
    in if null blocks then [0] else map length blocks

constraints :: Board -> Constraints
constraints (Board arr) =
    let ((1,1), (m,n)) = A.bounds arr
        rows = [ [arr A.! (i,j) | j <- [1..n]] | i <- [1..m] ]
        cols = [ [arr A.! (i,j) | i <- [1..m]] | j <- [1..n] ]
    in Constraints (map lineConstraint rows) (map lineConstraint cols)

generateAllBoards (m,n) =
    [ Board $ A.listArray ((1,1),(m,n)) xs | xs <- replicateM (m*n) [Empty, Full] ]
