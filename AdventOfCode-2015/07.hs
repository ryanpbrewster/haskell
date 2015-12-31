-- 07.hs
import qualified Data.Graph as G
import qualified Data.Map as M
import Data.Bits
import Text.ParserCombinators.Parsec
import Data.List (partition)
import Data.Maybe (mapMaybe)
import Control.Monad

main = do
  nodes <- fmap (map parseNode . lines) (readFile "07.input")
  let a = (solve nodes) M.! "a"
  let nodes2 = [ n | n@(Node nId _) <- nodes, nId /= NodeId "b" ] ++ [ Node (NodeId "b") (Derived0 $ ConstSource a) ]
  let a' = (solve nodes2) M.! "a"
  print a
  print a'
  

data Source = Node String | Const Int deriving (Show)

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
sources (Derived0 s) = [s]
sources (Derived1 _ s) = [s]
sources (Derived2 _ a b) = [a, b]

data Op1 = Not deriving (Show)
data Op2 = And | Or | LeftShift | RightShift deriving (Show)

data Node = Node NodeId NodeInput deriving (Show)

solve :: [Node] -> M.Map NodeId Int
solve ns = solve' ns M.empty
  where solve' [] acc = acc
        solve' toBeProcessed acc = 
          let (canBeProcessed, rest) = partition (evaluable acc) toBeProcessed
              processed = M.fromList [ (nId, evaluate acc nInp) | Node nId nInp <- canBeProcessed ]
          in solve' rest (M.union acc processed)


evaluable :: M.Map NodeId Int -> Node -> Bool
evaluable acc (Node _ nInp) =
  all (`M.member` acc) (mapMaybe asNode $ sources nInp)

evaluate :: M.Map NodeId Int -> NodeInput -> Int
evaluate acc nInp = case nInp of Derived0 s -> eval s
                                 Derived1 Not s -> complement (eval s)
                                 Derived2 And a b -> (eval a) .&. (eval b)
                                 Derived2 Or a b -> (eval a) .|. (eval b)
                                 Derived2 LeftShift a b -> (eval a) `shiftL` (eval b)
                                 Derived2 RightShift a b -> (eval a) `shiftR` (eval b)
  where eval s = case s of ConstSource v -> v
                           NodeSource nId -> acc M.! nId

{-
######################
## PARSING BULLSHIT ##
######################
-}

parseNode inp = case parse pNode "" inp of
  Left _ -> error $ "Could not parse: " ++ show inp
  Right v -> v

pNode :: Parser Node
pNode = do
  nodeInput <- pNodeInput
  string " -> "
  nodeId <- pNodeId
  return $ Node nodeId nodeInput

pNodeInput :: Parser NodeInput
pNodeInput = choice $ map try [ pDerived2, pDerived1, pDerived0 ]

pSource :: Parser Source
pSource = choice $ map try [ pInt >>= return . ConstSource
                           , pNodeId >>= return . NodeSource
                           ]

pDerived0 :: Parser NodeInput
pDerived0 = do
  v <- pSource
  return $ Derived0 v

pDerived1 :: Parser NodeInput
pDerived1 = do
  op <- pOp1
  spaces
  s <- pSource
  return $ Derived1 op s

pDerived2 :: Parser NodeInput
pDerived2 = do
  a <- pSource
  spaces
  op <- pOp2
  spaces
  b <- pSource
  return $ Derived2 op a b

pOp1 :: Parser Op1
pOp1 = choice [ string "NOT" >> return Not ]

pOp2 :: Parser Op2
pOp2 = choice [ string "AND" >> return And
              , string "OR" >> return Or
              , string "LSHIFT" >> return LeftShift
              , string "RSHIFT" >> return RightShift
              ]

pInt :: Parser Int
pInt = do
  v <- many1 digit
  return $ read v

pNodeId :: Parser NodeId
pNodeId = do
  nid <- many lower
  return $ NodeId nid
