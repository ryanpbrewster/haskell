-- a_pile_of_bricks.hs
{-
 -  You have a pile of bricks. Every brick has it's index number and
 -  coordinates of opposite vertices.
 -
 -  You know that somewhere on the wall there is a rectangular hole, and you
 -  are given coordinates of opposite vertices of that hole.
 -
 -  Determine which bricks may pass through that hole.
 -
 -  In situations where brick and hole have an equal sizes, we assume that it
 -  can pass through this hole.
 -
 -  All the holes are two-dimensional. All of the bricks are three-dimensional.
 -
 -  Input sample:
 -      [4,3] [3,-3]|(1 [10,9,4] [9,4,2])
 -      [-1,-5] [5,-2]|(1 [4,7,8] [2,9,0]);(2 [0,7,1] [5,9,8])
 -      [-4,-5] [-5,-3]|(1 [4,8,6] [0,9,2]);(2 [8,-1,3] [0,5,4])
 -
 -  Your program should accept as its first argument a path to a filename. The input file contains several lines. Each line is one test case.
 -
 -  Each line contains coordinates of opposite vertices of a hole (before the
 -  vertical bar) separated by space bar and the list of bricks you need to
 -  check. Each brick is enclosed in parentheses where the 1st number is
 -  a brick's index number, the 2nd and 3rd group of numbers are brick's
 -  coordinates of opposite vertices (separated by a space bar), each brick is
 -  divided by semicolon. E.g.
 -
 -  Output sample:
 -      1
 -      1,2
 -      -
 -
 -  For each set of bricks produce a list of bricks (their index numbers in ascending order separated by comma) that can pass through the hole. E.g.
 -
 -  Constraints:
 -  Coordinates are in range [-100, 100]
 -  There might be up to 15 bricks you need to check. 
 -}

import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import qualified Data.Map as M
import Data.List (intercalate)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

{- Data representation and parsing -}
data Problem = Problem Rect (M.Map Int Brick) deriving (Show)
data Rect = Rect Point2D Point2D deriving (Show)
data Brick = Brick Point3D Point3D deriving (Show)
data Point2D = Point2D Double Double deriving (Show, Eq, Ord)
data Point3D = Point3D Double Double Double deriving (Show, Eq, Ord)

newRect :: Point2D -> Point2D -> Rect
newRect p1 p2 = Rect (min p1 p2) (max p1 p2)


newBrick :: Point3D -> Point3D -> Brick
newBrick p1 p2 = Brick (min p1 p2) (max p1 p2)

parseProblem :: String -> Problem
parseProblem inp = case parse pProblem "" inp of
    Right v -> v
    Left _ -> error $ "Could not parse " ++ show inp

pProblem = do
    rect <- pRect
    spaces
    char '|'
    bricks <- pBricks
    return $ Problem rect bricks

pRect = do
    spaces
    p1 <- pPoint2D
    spaces
    p2 <- pPoint2D
    return $ newRect p1 p2

pBricks = do
    bricks_with_idxs <- sepBy pBrickWithIndex (char ';')
    return $ M.fromList (bricks_with_idxs :: [(Int,Brick)])

pBrickWithIndex = do
    spaces
    char '('
    idx <- pInt
    spaces
    p1 <- pPoint3D
    spaces
    p2 <- pPoint3D
    char ')'
    return $ (idx, newBrick p1 p2)

pPoint2D = do
    char '['
    x <- pNum
    char ','
    y <- pNum
    char ']'
    return $ Point2D x y

pPoint3D = do
    char '['
    x <- pNum
    char ','
    y <- pNum
    char ','
    z <- pNum
    char ']'
    return $ Point3D x y z

pInt = do
    s <- many digit
    return $ read s

pNum = do
    s <- many (oneOf "-0123456789.")
    return $ read s

showAnswer :: [Int] -> String
showAnswer [] = "-"
showAnswer xs = intercalate "," $ map show xs

solveProblem txt =
    let inps = map parseProblem $ lines txt
        anss = map bricksThatFit inps
    in unlines $ map showAnswer anss


{- Actual code -}
faces :: Brick -> [Rect]
faces (Brick (Point3D x1 y1 z1) (Point3D x2 y2 z2)) =
    [ newRect (Point2D x1 y1) (Point2D x2 y2)
    , newRect (Point2D x1 z1) (Point2D x2 z2)
    , newRect (Point2D y1 z1) (Point2D y2 z2)
    ]

smallerThan :: Rect -> Rect -> Bool
r1 `smallerThan` r2 =
    (width r1 <= width r2) && (height r1 <= height r2)

width (Rect (Point2D x1 y1) (Point2D x2 y2))  = abs $ x2 - x1
height (Rect (Point2D x1 y1) (Point2D x2 y2)) = abs $ y2 - y1

brick `fitsIn` hole = any (`smallerThan` hole) (faces brick)

bricksThatFit (Problem hole brick_map) =
    [ idx | (idx, brick) <- M.toList brick_map, brick `fitsIn` hole ]
