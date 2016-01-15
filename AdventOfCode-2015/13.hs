-- 09.hs
import Text.ParserCombinators.Parsec
import Data.List (tails, permutations, minimumBy, maximumBy, nub, last)
import Data.Ord (comparing)
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Vector as V

data Klique = Klique (V.Vector Vertex) (A.Array (Int, Int) Int) deriving (Show)
data Vertex = Vertex String deriving (Eq, Ord, Show)
data Edge = Edge Vertex Vertex Int deriving (Show)

main = do
  edges <- fmap (map parseEdge . lines) (readFile "13.input")
  let verts = V.fromList $ nub $ concat [ [a,b] | Edge a b _ <- edges ] 
  let k1 = buildGraph verts edges
  let k2 = buildGraph (V.cons (Vertex "ryan") verts) edges
  print k1
  print k2
  print $ bestArrangement k1
  print $ bestArrangement k2

buildGraph :: V.Vector Vertex -> [Edge] -> Klique
buildGraph verts edges =
  let n = V.length verts
      idxByVert = M.fromList [ (v, idx) | (idx, v) <- V.toList $ V.indexed verts ]
      weightsWithIdx = [ ((idxByVert M.! a, idxByVert M.! b), w) | Edge a b w <- edges ]
      weightMatrix = A.accumArray (+) 0 ((0,0),(n-1,n-1)) weightsWithIdx
  in Klique verts weightMatrix

bestArrangement :: Klique -> Int
bestArrangement (Klique verts weightMatrix) =
  let n = V.length verts
      arrangements = permutations [0..n-1]
      scorePair :: (Int, Int) -> Int
      scorePair (i,j) = weightMatrix A.! (i,j) + weightMatrix A.! (j,i)
      score :: [Int] -> Int
      score xs = sum $ map scorePair $ pairs xs
  in maximum $ map score arrangements

pairs [] = []
pairs [x] = []
pairs [x,y] = [(x,y)]
pairs xs = (head xs, last xs) : [ (x,y) | (x:y:_) <- tails xs ]

{-
######################
## Parsing Bullshit ##
######################
-}

parseEdge inp = case parse pEdge "" inp of
  Left _ -> error $ "Could not parse Edge from: " ++ show inp
  Right v -> v

pEdge :: Parser Edge
pEdge = do
  a <- pVertex
  string " would "
  sign <- pSign
  spaces
  w <- pInt
  string " happiness units by sitting next to "
  b <- pVertex
  return $ Edge a b (sign * w)

pVertex :: Parser Vertex
pVertex = many1 letter >>= return . Vertex

pInt :: Parser Int
pInt = many1 digit >>= return . read

pSign :: Parser Int
pSign = choice $ map try [ string "lose" >> return (-1)
                         , string "gain" >> return   1 ]
