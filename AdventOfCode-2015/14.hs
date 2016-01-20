-- 14.hs
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
import Text.ParserCombinators.Parsec
import Data.List (maximumBy, transpose, group, sort)
import Data.Ord (comparing)
import qualified Data.Map as M

type Name = String
type Speed = Int
type Time = Int
type Distance = Int
data Reindeer where
  Reindeer :: Name -> Speed -> Time -> Time -> Reindeer
  deriving (Show, Ord, Eq)

main = do
  let raceTime = 2503
  reindeers <- fmap (map parseReindeer . lines) (readFile "14.input")
  let distances = M.fromList [ (r, reindeerDistance r) | r <- reindeers ]
  let longestDistance = maximum $ map (!! raceTime) (M.elems distances)
  print longestDistance

  let leadingReindeers = [ [ r | (r, d) <- zip reindeers dists, d == maximum dists ]
                         | dists <- transpose (M.elems distances) ]
  let reindeerPoints = scanl (M.unionWith (+)) (M.fromList $ zip reindeers (repeat 0)) [ M.fromList $ zip winners (repeat 1) | winners <- leadingReindeers ]
  print $ maximum (M.elems (reindeerPoints !! raceTime)) - 1

amortizedSpeed :: Reindeer -> Double
amortizedSpeed (Reindeer _ speed uptime downtime) =
  fromIntegral (speed * uptime) / fromIntegral (uptime + downtime)

reindeerDistance :: Reindeer -> [Distance]
reindeerDistance (Reindeer _ speed uptime downtime) =
  let distanceTravelled = cycle $ replicate uptime speed ++ replicate downtime 0
  in scanl (+) 0 distanceTravelled


parseReindeer inp = case parse pReindeer "" inp of
  Left _ -> error $ "Could not parse input: " ++ show inp
  Right v -> v

pReindeer :: Parser Reindeer
pReindeer = do
  name <- pName
  string " can fly "
  speed <- pSpeed
  string " for "
  uptime <- pTime
  string ", but then must rest for "
  downtime <- pTime
  string "."
  return $ Reindeer name speed uptime downtime

pName = many letter
pSpeed = do
  v <- pInt
  spaces >> string "km/s"
  return v

pTime = do
  v <- pInt
  spaces >> string "seconds"
  return v

pInt :: Parser Int
pInt = many digit >>= return . read
