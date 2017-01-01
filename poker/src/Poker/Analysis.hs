module Poker.Analysis
( bestHand
) where

import Data.Maybe (catMaybes, listToMaybe)
import Data.List (find, foldl', sortOn, tails)
import Data.Ord (Down(..))

import Data.Set (Set)
import qualified Data.Set as S

import Data.Map (Map)
import qualified Data.Map as M

import Poker.Data (Card(..), Rank(..), Hand(..), allRanks, allSuits)

bestHand :: Set Card -> Hand
bestHand card_set =
  foldl' max (extractHighCard card_set) $
    catMaybes [ fn card_set | fn <- [ extractStraightFlush
                                    , extractSet
                                    , extractFullHouse
                                    , extractStraight
                                    , extractFlush ] ]

extractHighCard :: Set Card -> Hand
extractHighCard card_set = HighCard $ maximum [ rank | Card rank suit <- S.toList card_set ]

extractStraightFlush :: Set Card -> Maybe Hand
extractStraightFlush card_set =
  let
    straights = chunksOf 5 (reverse allRanks)
    straight_flushes =
       [ [ Card rank suit | rank <- straight ] | straight <- straights, suit <- allSuits ]
    best_straight_flush = find (all (`S.member` card_set)) straight_flushes
  in fmap (\((Card rank suit):_) -> StraightFlush rank) best_straight_flush

extractSet :: Set Card -> Maybe Hand
extractSet card_set =
  let 
    count_by_rank = M.fromListWith (+) [ (rank, 1) | Card rank _ <- S.toList card_set ]
    sets = sortOn Down [ (count, rank) | (rank, count) <- M.toList count_by_rank ]
  in case sets of
    (4, r):_ -> Just $ Quartet r
    (3, r):_ -> Just $ Triple r
    (2, r1):(2, r2):_ -> Just $ TwoPair r1 r2
    (2, r):_ -> Just $ OnePair r
    _ -> Nothing

extractFullHouse :: Set Card -> Maybe Hand
extractFullHouse card_set =
  let 
    count_by_rank = (M.fromListWith (+) [ (rank, 1) | Card rank _ <- S.toList card_set ]) :: Map Rank Int
    useful_sets = M.filter (\count -> count >= 2 && count <= 3) count_by_rank
    sets = sortOn Down [ (count, rank) | (rank, count) <- M.toList useful_sets ]
  in case sets of
    (3, r1):rest@(_:_) -> Just (FullHouse r1 $ maximum $ map snd rest)
    _ -> Nothing
      

extractStraight :: Set Card -> Maybe Hand
extractStraight card_set =
  let
    rank_set = S.map (\(Card rank suit) -> rank) card_set
    straights = chunksOf 5 (reverse allRanks)
    best_straight = find (all (`S.member` rank_set)) straights
  in fmap (\(rank:_) -> Straight rank) best_straight

extractFlush :: Set Card -> Maybe Hand
extractFlush card_set =
  let
    count_by_suit = M.fromListWith (+) [ (suit, 1) | Card _ suit <- S.toList card_set ]
    flush_suits = M.keysSet (M.filter (>= 5) count_by_suit)
  in if null flush_suits then Nothing else 
    Just (Flush $ maximum [ rank | Card rank suit <- S.toList card_set, S.member suit flush_suits ])

chunksOf n xs = takeWhile (\ys -> length ys == n) $ map (take n) $ tails xs
