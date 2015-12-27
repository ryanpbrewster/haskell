-- 01.hs
import Data.Maybe (fromJust)
import Data.List (findIndex)

main = do
  floorChanges <- fmap (map floorChange) (readFile "01.input")
  print $ solvePart1 floorChanges
  print $ fromJust $ solvePart2 floorChanges

floorChange :: Char -> Int
floorChange '(' =  1
floorChange ')' = -1

solvePart1 :: [Int] -> Int
solvePart1 = sum

solvePart2 :: [Int] -> Maybe Int
solvePart2 inp =
  let floors = scanl (+) 0 inp
  in findIndex (<0) floors
