-- 06.hs
import Text.ParserCombinators.Parsec
import Data.Array

main = do
  let n = 1000
  instructions <- fmap (map parseInstruction . lines) (readFile "06.input")
  let initGrid1 = Grid1 (listArray (Position (0,0), Position (n-1,n-1)) (repeat Off))
  let finalOnCount = fst $ getOnOffCounts $ foldl applyInstruction1 initGrid1 instructions
  print finalOnCount
  let initGrid2 = Grid2 (listArray (Position (0,0), Position (n-1,n-1)) (repeat 0))
  let finalBrightness = getBrightness $ foldl applyInstruction2 initGrid2 instructions
  print finalBrightness

data Status = On | Off deriving (Eq)

data Grid1 = Grid1 (Array Position Status)
data Grid2 = Grid2 (Array Position Int)

data Instruction = Instruction Action Selection deriving (Show)
data Action = TurnOn | TurnOff | Toggle deriving (Show)
data Selection = Selection Position Position deriving (Show)
newtype Position = Position (Int, Int) deriving (Show, Ord, Eq, Ix)

toggle On = Off
toggle Off = On

getOnOffCounts (Grid1 arr) =
  let es = elems arr
      onCount = length $ filter (== On) es
      offCount = length $ filter (== Off) es
  in (onCount, offCount)

getBrightness (Grid2 arr) = sum $ elems arr

applyInstruction1 :: Grid1 -> Instruction -> Grid1
applyInstruction1 (Grid1 arr) (Instruction act (Selection ll ur)) =
  let affectedPositions = range (ll, ur)
      change = case act of
        TurnOn -> const On
        TurnOff -> const Off
        Toggle -> toggle
  in Grid1 $ arr // (zip affectedPositions (map (change . (arr !)) affectedPositions))

applyInstruction2 :: Grid2 -> Instruction -> Grid2
applyInstruction2 (Grid2 arr) (Instruction act (Selection ll ur)) =
  let affectedPositions = range (ll, ur)
      change = case act of
        TurnOn -> \x -> x+1
        TurnOff -> \x -> max (x-1) 0
        Toggle -> \x -> x+2
  in Grid2 $ arr // (zip affectedPositions (map (change . (arr !)) affectedPositions))

parseInstruction :: String -> Instruction
parseInstruction inp = case parse pInstruction "" inp of
  Right v -> v
  Left _ -> error $ "Could not parse: " ++ show inp

pInstruction = do
  a <- pAction
  spaces
  s <- pSelection
  return $ Instruction a s

pAction =
  choice $ map try [ string "turn on" >> return TurnOn
                   , string "turn off" >> return TurnOff
                   , string "toggle" >> return Toggle
                   ]

pSelection = do
  ll <- pPosition
  spaces
  string "through"
  spaces
  ur <- pPosition
  return $ Selection ll ur

pPosition = do
  x <- pNumber
  char ','
  spaces
  y <- pNumber
  return $ Position (x, y)

pNumber = do
  n <- many digit
  return $ read n
