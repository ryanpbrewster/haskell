module Main where

import Poker.Data
import Poker.Analysis
import qualified Data.Set as S

import System.Random
import System.Random.Shuffle

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as M

import Data.Ord (Down(..))
import Data.List (sortOn)

trials = 10000

main :: IO ()
main = do
  forM_ [5..30] $ \num_cards -> do
    decks <- replicateM trials $ fmap (S.fromList . take num_cards) (shuffleM allCards)
    let hand_tally = M.fromListWith (+) $ zip (map (handType . bestHand) decks) (repeat 1)
    let tally_dump = unlines [ hand ++ "\t" ++ show (M.findWithDefault 0 hand hand_tally) | hand <- allTypes ]
    writeFile (show num_cards ++ ".log") tally_dump


handType :: Hand -> String
handType (HighCard r) = "HighCard"
-- handType (OnePair r) = "OnePair"
handType (TwoPair r1 r2) = "TwoPair"
handType (Triple r) = "Triple"
handType (Straight r) = "Straight"
handType (Flush r) = "Flush"
handType (FullHouse r1 r2) = "FullHouse"
handType (Quartet r) = "Quartet"
handType (StraightFlush r) = "StraightFlush"

allTypes = [ "HighCard"
           , "OnePair"
           , "TwoPair"
           , "Triple"
           , "Straight"
           , "Flush"
           , "FullHouse"
           , "Quartet"
           , "StraightFlush"
           ]
