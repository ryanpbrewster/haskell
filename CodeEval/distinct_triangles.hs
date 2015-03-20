-- distinct_triangles
{-
 -
 - Alice the archaeologist has just entered the tomb of the Pharaoh. She turns on
 - her flashlight and notices an undirected graph painted on the wall, with
 - V nodes and E edges. Suddenly, the stone door behind her slams shut.
 - Fortunately, Alice knows the way out - she must place N pebbles upon the altar
 - to re-open the door, where N is the number of triangles in the graph.
 -
 - For example:
 -
 - N is 2 in this graph.
 -
 - The first argument is a file with different test cases. Each test case begins
 - with two integers, V and E (1 <= V, E <= 100), separated by a space and
 - finishes with following symbol ";". Then, E edges, which represented as two
 - integers separated by space, Each edge is comma separated. Each vertex is in
 - the range (0 <= vertex < V).
 -
 - Print out the number of distinct triangles formed over three vertices and edges
 - in the graph.
 -
 - Sample input:
 -     4 5;0 2,0 1,1 2,1 3,2 3
 -     9 3;1 3,1 8,3 8
 -     9 3;5 6,5 7,6 7
 -
 - Constraints:
 -     1 <= V, E <= 100
 -     0 <= vertex < V
 -     Number of test cases is 10.
 -}

import qualified Data.Set as S
import Text.ParserCombinators.Parsec
import System.Environment (getArgs)

main = do
    args <- getArgs
    txt <- readFile (head args)
    putStr $ solveProblems txt

solveProblems txt =
    let lns = lines txt
        inps = map parseInput lns
        anss = map solveProblem inps
    in unlines $ map show anss


solveProblem edges =
    let v = numberOfConnectedVertices edges
        e = length edges
    in numberOfTriangles v e

-- The Euler characteristic of any planar graph is 2
-- That means that V - E + F = 2
-- We do not care about the "external" face, so we want F-1
-- Thus, numberOfTriangles = (2+E-V) - 1
numberOfTriangles :: Int -> Int -> Int
numberOfTriangles v e = 1 + e - v

type Vertex = Int
type Edge = (Vertex, Vertex)

numberOfConnectedVertices :: [Edge] -> Int
numberOfConnectedVertices edges =
    S.size $ S.fromList $ concat [ [u,v] | (u,v) <- edges ]


parseInput :: String -> [Edge]
parseInput input = case parse pInput "" input of
    Right v -> v
    Left _  -> error $ "Could not parse: " ++ show input

pInput = do
    pNum >> space >> pNum >> char ';'
    edges <- sepBy pEdge (char ',')
    return edges

pEdge = do
    u <- pNum
    space
    v <- pNum
    return (u,v)

pNum = do
    n <- many (oneOf "0123456789")
    return $ read n
