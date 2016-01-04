-- 09.hs
import Text.ParserCombinators.Parsec
import Data.List (tails, permutations, minimumBy, nub)
import Data.Ord (comparing)
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Vector as V

data Klique = Klique (V.Vector Vertex) (A.Array (Int, Int) Weight)
data Vertex = Vertex String deriving (Eq, Ord, Show)
data Edge = Edge Vertex Vertex Weight
newtype Path = Path [Edge]
newtype Weight = Weight Int deriving (Eq, Ord, Show)

main = do
  graph <- fmap (buildGraph . map parseEdge . lines) (readFile "09.input")
  print $ pathLength $ travelingSalesman graph

sliding k xs = takeWhile (\w -> length w == k) $ map (take k) (tails xs)
pathLength (Path es) = Weight $ sum [ w | Edge a b (Weight w) <- es ]

travelingSalesman :: Klique -> Path
travelingSalesman (Klique verts weightMatrix) =
    let n = V.length verts
        allPossiblePaths = map vertexPath (permutations [0..n-1])
    in minimumBy (comparing pathLength) allPossiblePaths
  where vertexPath idxs = Path [ Edge (verts V.! i) (verts V.! j) (weightMatrix A.! (i,j)) | [i,j] <- sliding 2 idxs ]

buildGraph :: [Edge] -> Klique
buildGraph edges =
  let verts = V.fromList $ nub $ concat [ [a,b] | Edge a b w <- edges ]
      n = V.length verts
      idxByVert = M.fromList [ (v, idx) | (idx, v) <- V.toList $ V.indexed verts ]
      weightsWithIdx = [ ((idxByVert M.! a, idxByVert M.! b), w) | Edge a b w <- edges ]
      weightMatrix = A.array ((0,0),(n-1,n-1)) $ concat [ [((i,j),w),((j,i),w)] | ((i,j),w) <- weightsWithIdx ]
  in Klique verts weightMatrix

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
  string " to "
  b <- pVertex
  string " = "
  w <- pWeight
  return $ Edge a b w

pVertex :: Parser Vertex
pVertex = many1 letter >>= return . Vertex

pWeight :: Parser Weight
pWeight = pInt >>= return . Weight

pInt :: Parser Int
pInt = many1 digit >>= return . read
