import Data.Maybe( isJust )
import Control.Monad( replicateM )

main = do
    num_tests <- readLn :: IO Int
    inputs <- replicateM num_tests readInput
    mapM_ (putStrLn . solveInstance) inputs

-- discard one line, then split the next line into words and parse them
readInput :: IO [Int]
readInput = getLine >> fmap (map read . words) getLine

solveInstance :: [Int] -> String
solveInstance = formatOutput . isValidPreorder

formatOutput True  = "YES"
formatOutput False = "NO"

{-
 - Bulk of the solution is below
 -}

data Node a = Node a (Node a) (Node a)
            | Empty
            deriving (Show)

toPreorder Empty = []
toPreorder (Node v left right) =
    [v] ++ toPreorder left ++ toPreorder right

fromPreorder [] = Just Empty
fromPreorder vv =
    let lo = minimum vv - 1
        hi = maximum vv + 1
        (tree, leftovers) = fromPreorderHelper vv lo hi
    in if null leftovers then Just tree else Nothing

fromPreorderHelper [] _ _ = (Empty, [])
fromPreorderHelper vv@(v:vs) lo hi
    | lo < v && v < hi =
            let (left,  vs')  = fromPreorderHelper vs  lo v
                (right, vs'') = fromPreorderHelper vs' v  hi
            in (Node v left right, vs'')
    | otherwise = (Empty, vv)

isValidPreorder = isJust . fromPreorder
