-- skyscrapers.hs
{-
 - SKYSCRAPERS
 - CHALLENGE DESCRIPTION:
 -
 - You are to design a program which helps you in drawing the skyline of a city. You are given the locations of the buildings represented by triples (L, H, R) where L and R are left and right coordinates of the building and H is the height of the building. All buildings are rectangular in shape and they are standing on a very flat surface. E.g. 
 -  
 -   
 -   On the first diagram the buildings are represented by the following triples:
 -
 -   (1,2,3); (2,4,6); (4,5,5); (7,3,11); (9,2,14); (13,7,15); (14,3,17)
 -   The drawing line as shown on the second diagram is described by the following sequence:
 -
 -   0 2 2 4 4 5 5 4 6 0 7 3 11 2 13 7 15 3 17 0
 -   INPUT SAMPLE:
 -
 -   Your program should accept as its first argument a path to a filename. Each line in this file is one test case. Each test case will contain the list of triples semicolon separated. E.g.
 -
 -   (1,2,3); (2,4,6); (4,5,5); (7,3,11); (9,2,14); (13,7,15); (14,3,17)
 -   (2,22,3);(6,12,10);(15,6,21)
 -   (1,2,6);(9,23,22);(22,6,24);(8,14,19);(23,12,30)
 -   OUTPUT SAMPLE:
 -
 -   The output must describe the drawing line as a vector (X1,H1,X2,H2,X3,H3,X3,Xn-1,Hn-1,Xn) where X is a x-coordinate of a point where the line is changing its direction from horizontal to vertical, and H is a height of the vertical line. You're drawing continuously by starting at the bottom of the first left building and finishing at the bottom of the right building. So for each test case print out the drawing line in a way as it is shown below.
 -
 -   0 2 2 4 4 5 5 4 6 0 7 3 11 2 13 7 15 3 17 0
 -   2 22 3 0 6 12 10 0 15 6 21 0
 -   1 2 6 0 8 14 9 23 22 6 23 12 30 0
 -   Notice that the elimination of hidden lines is one of the problems that appear in CAD (computer-aided design). 
 -
 -   Constraints: 
 -   H in range (1, 100), max(x-coordinate) <= 10000, number of buildings <= 1000
 -
 -}


import System.Environment (getArgs)
import Data.List (delete, sort)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblem txt

wordsBy pred s = wordsBy' pred $ dropWhile pred s
    where wordsBy' _ [] = []
          wordsBy' pred s = let (f,r) = break pred s
                            in f:wordsBy' pred (dropWhile pred r)

solveProblem txt = let lns = lines txt
                       inputs = map parseInput lns
                       anss = map optimalSkyline inputs
                   in unlines $ map show anss

-- roll 3 [1,2,3,4,5,6,7,8,...] = [ [1,2,3], [4,5,6], [7,8,9], ...]
roll _ [] = []
roll k xs = let (f,r) = splitAt k xs in f:(roll k r)

data EventT = Event Int Int TypeT deriving (Show, Ord, Eq)
data TypeT = L | R deriving (Show, Eq, Ord)

parseInput :: String -> [EventT]
parseInput ln = let strs = wordsBy (`elem` "() ,;") ln
                    vals = roll 3 $ map read strs
                in concat [ [ Event l h L, Event r h R] | [l,h,r] <- vals ]




optimalSkyline es = optimalSkyline' [] (sort es)


optimalSkyline' [] [] = []
optimalSkyline' hs (Event x h L:es) =
    let skyline = optimalSkyline' (h:hs) es
    in if (null hs || h > maximum hs) then (Event x h L):skyline else skyline
optimalSkyline' hs (Event x h R:es) =
    let skyline = optimalSkyline' (delete h hs) es
    in if h == maximum hs then (Event x h R):skyline else skyline
