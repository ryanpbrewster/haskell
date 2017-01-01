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

trials = 1000

main :: IO ()
main = do
  forM_ [5..25] $ \num_cards -> do
    print num_cards
    decks <- replicateM trials $ fmap (S.fromList . take num_cards) (shuffleM allCards)
    let hand_tally = M.fromListWith (+) $ zip (map bestHand decks) (repeat 1) :: Map Hand Int
    forM_ (sortOn Down $ M.toList hand_tally) $ \(hand, count) ->
      putStrLn $ "\t" ++ show count ++ "\t" ++ show hand
