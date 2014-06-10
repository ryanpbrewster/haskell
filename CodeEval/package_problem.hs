-- package_problem.hs
{-
 -
 - Package Problem
 - Challenge Description:
 -
 - You want to send your friend a package with different things.
 - Each thing you put inside of a package has such parameters as index number, weight and cost.
 - The package has a weight limitation.
 - Your goal is to determine which things to put into the package so that the
 - total weight is less than or equal to the package limit and the total cost
 - is as large as possible.
 -
 - You would prefer to send a package which has less weight in case there is
 - more than one package with the same price.
 -
 - This is a variation of Knapsack problem
 - Input sample:
 -
 - Your program should accept as its first argument a path to a filename. The
 - input file contains several lines. Each line is one test case.
 -
 - Each line contains the weight that a package can take (before the colon) and
 - the list of things you need to pick from. Each thing is enclosed in
 - parentheses where 1st number is a thing's index number, the 2nd is it's
 - weight and the 3rd is it's cost. E.g.
 -
 - 81 : (1,53.38,$45) (2,88.62,$98) (3,78.48,$3) (4,72.30,$76) (5,30.18,$9) (6,46.34,$48)
 - 8 : (1,15.3,$34)
 - 75 : (1,85.31,$29) (2,14.55,$74) (3,3.98,$16) (4,26.24,$55) (5,63.69,$52) (6,76.25,$75) (7,60.02,$74) (8,93.18,$35) (9,89.95,$78)
 - 56 : (1,90.72,$13) (2,33.80,$40) (3,43.15,$10) (4,37.97,$16) (5,46.81,$36) (6,48.77,$79) (7,81.80,$45) (8,19.36,$79) (9,6.76,$64)
 -
 - Output sample:
 -
 - For each set of things produce a list of things (their index numbers separated by comma) that you put into the package. E.g.
 -
 - 4
 - -
 - 2,7
 - 8,9
 -
 - Constraints:
 - Max weight any package can take is <= 100.
 - There might be up to 15 things you need to choose from.
 - Max weight and max cost of any thing is <= 100.
 -}

import System.Environment (getArgs)
import Text.ParserCombinators.Parsec
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

{-----------------}
{- Problem Setup -}
{-----------------}

main = do
    args <- getArgs
    txt <- readFile $ head args
    putStr $ solveProblem txt

solveProblem :: String -> String
solveProblem txt =
    let lns = filter (not.null) $ lines txt
        inps = map parseProblem lns
        anss = map knapsack inps
        outs = map ppSolution anss
    in unlines outs

ppSolution :: [Item] -> String
ppSolution []  = "-"
ppSolution its = intercalate "," $ map (ppIndex . getIndex) its

foo = "8 : (1,15.3,$34)"

knapsack :: Problem -> [Item]
knapsack (Problem cap items) = knapsack' cap items
    where knapsack' u [] = []
          knapsack' u (it:its)
              | getWeight it > u = knapsack' u its
              | otherwise        = let w = getWeight it
                                       u' = u `wminus` w
                                       useit  = it : knapsack' u' its
                                       loseit =      knapsack' u  its
                                   in maximumBy (comparing knapsackScore) [useit,loseit]

knapsackScore its = (totalValue its, wneg $ totalWeight its)


{--------------------}
{- Data definitions -}
{--------------------}
data Weight = Weight Double deriving (Show, Eq, Ord)
data Value = Value Double deriving (Show, Eq, Ord)

Weight a `wplus`  Weight b = Weight (a+b)
Weight a `wminus` Weight b = Weight (a-b)
wneg (Weight a) = Weight (-a)

Value a `vplus`  Value b = Value (a+b)
Value a `vminus` Value b = Value (a-b)

data Index = Index Int deriving (Show, Eq, Ord)

data Problem = Problem { getCapacity :: Weight
                       , getItems    :: [Item]
                       } deriving (Show)
data Item = Item { getIndex  :: Index
                 , getValue  :: Value
                 , getWeight :: Weight
                 } deriving (Show, Eq, Ord)

totalValue :: [Item] -> Value
totalValue its = foldl vplus (Value 0) $ map getValue its

totalWeight :: [Item] -> Weight
totalWeight its = foldl wplus (Weight 0) $ map getWeight its

ppIndex (Index i) = show i
ppValue (Value v) = "$" ++ show v
ppWeight (Weight w) = show w ++ " lbs"
ppItem (Item idx v w) =
    ppIndex idx ++ " --- " ++ ppValue v ++ " @ " ++ ppWeight w


{---------------------}
{- Parsing Functions -}
{---------------------}
parseProblem :: String -> Problem
parseProblem input = case parse pProblem "" input of
    Right v -> v
    Left _  -> error $ "Improperly formatted input:" ++ show input

-- 81 : (1,53.38,$45) (2,88.62,$98) (3,78.48,$3) (4,72.30,$76) (5,30.18,$9) (6,46.34,$48)
pProblem = do
    cap <- pWeight
    spaces
    char ':'
    items <- many pItem
    return $ Problem cap items

pIndex = do
    i <- pNum
    return $ Index (read i)
pValue = do
    char '$'
    v <- pNum
    return $ Value (read v)
pWeight = do
    w <- pNum
    return $ Weight (read w)
pItem = do
    spaces
    char '('
    idx <- pIndex
    char ','
    wgt <- pWeight
    char ','
    val <- pValue
    char ')'
    return $ Item idx val wgt

pNum = many (oneOf "0123456789.")
