-- 02.hs
import Text.ParserCombinators.Parsec
main = do
  boxes <- fmap (map parseBox . lines) (readFile "02.input")
  print $ sum $ map requiredPaper boxes
  print $ sum $ map requiredRibbon boxes

data Box = Box { l :: Int, w :: Int, h :: Int } deriving (Eq)
instance Show Box where
  show (Box l w h) = show l ++ "x" ++ show w ++ "x" ++ show h

requiredPaper :: Box -> Int
requiredPaper (Box l w h) =
  let side_areas = [ l*w, w*h, h*l ]
  in 2 * sum side_areas + minimum side_areas

requiredRibbon :: Box -> Int
requiredRibbon (Box l w h) =
  let perimeters = [ 2*l + 2*w, 2*w + 2*h, 2*h + 2*l ]
      volume = l*w*h
  in minimum perimeters + volume

parseBox :: String -> Box
parseBox inp = case parse pBox "" inp of
  Right v -> v
  Left _ -> error $ "Could not parse input:" ++ show inp

pBox = do
  [l, w, h] <- sepBy pDimension (char 'x')
  return $ Box l w h

pDimension = read <$> many digit
