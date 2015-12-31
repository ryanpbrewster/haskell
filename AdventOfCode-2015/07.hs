-- 07.hs
import qualified Data.Graph as G
import qualified Data.Map as M
import Data.Bits
import Text.ParserCombinators.Parsec
import Data.List (partition)
import Data.Maybe (mapMaybe)
import Control.Monad

main = do
  nodes <- fmap (M.fromList . map parseNode . lines) (readFile "07.input")
  let a = (solve nodes) M.! "a"
  print a
  let nodes' = M.insert "b" (Const $ Value a) nodes
  let a' = (solve nodes') M.! "a"
  print a'
  
data Source = Node String | Value Int deriving (Show)

asNode :: Source -> Maybe String
asNode (Node nId) = Just nId
asNode _ = Nothing

data NodeInput = Const Source | Not Source 
               | LeftShift Source Source
               | RightShift Source Source
               | And Source Source
               | Or Source Source
               deriving (Show)

sources :: NodeInput -> [Source]
sources (Const s) = [s]
sources (Not s) = [s]
sources (LeftShift a b) = [a, b]
sources (RightShift a b) = [a, b]
sources (And a b) = [a, b]
sources (Or a b) = [a, b]

solve :: M.Map String NodeInput -> M.Map String Int
solve ns = solve' ns M.empty
  where solve' toBeProcessed acc
          | M.null toBeProcessed = acc
          | otherwise =
            let (canBeProcessed, rest) = M.partition (evaluable acc) toBeProcessed
                processed = M.map (evaluate acc) canBeProcessed
            in solve' rest (M.union acc processed)


evaluable :: M.Map String Int -> NodeInput -> Bool
evaluable acc nInp =
  all (`M.member` acc) (mapMaybe asNode $ sources nInp)

evaluate :: M.Map String Int -> NodeInput -> Int
evaluate acc nInp = case nInp of Const s -> eval s
                                 Not s -> complement (eval s)
                                 And a b -> (eval a) .&. (eval b)
                                 Or a b -> (eval a) .|. (eval b)
                                 LeftShift a b -> (eval a) `shiftL` (eval b)
                                 RightShift a b -> (eval a) `shiftR` (eval b)
  where eval s = case s of Value v -> v
                           Node nId -> acc M.! nId

{-
######################
## PARSING BULLSHIT ##
######################
-}

parseNode inp = case parse pNode "" inp of
  Left _ -> error $ "Could not parse: " ++ show inp
  Right v -> v

pNode :: Parser (String, NodeInput)
pNode = do
  nodeInput <- pNodeInput
  string " -> "
  nodeId <- pNodeId
  return (nodeId, nodeInput)

pSource :: Parser Source
pSource = choice $ map try [ pInt >>= return . Value , pNodeId >>= return . Node ]

pInt :: Parser Int
pInt = many1 digit >>= return . read

pNodeId :: Parser String
pNodeId = many1 lower >>= return

pNodeInput :: Parser NodeInput
pNodeInput =
  choice $ map try [ do string "NOT"; spaces; s <- pSource; return $ Not s
                   , do a <- pSource; string " AND "; b <- pSource; return $ And a b
                   , do a <- pSource; string " OR "; b <- pSource; return $ Or a b
                   , do a <- pSource; string " LSHIFT "; b <- pSource; return $ LeftShift a b
                   , do a <- pSource; string " RSHIFT "; b <- pSource; return $ RightShift a b
                   , do s <- pSource; return $ Const s
                   ]
