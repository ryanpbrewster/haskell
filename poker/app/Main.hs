module Main where

import Poker.Data
import Poker.Analysis
import qualified Data.Set as S

main :: IO ()
main = do
  print ("royal flush"
        , bestHand $ S.fromList [ Card Ten Hearts
                                , Card Queen Hearts
                                , Card Ace Hearts
                                , Card King Hearts
                                , Card Jack Hearts
                                ])
  print ("6-high straight flush"
        , bestHand $ S.fromList [ Card Two Hearts
                                , Card Three Hearts
                                , Card Four Hearts
                                , Card Five Hearts
                                , Card Six Hearts
                                ])
  print ("4x jacks"
        , bestHand $ S.fromList [ Card Jack Clubs
                                , Card Jack Diamonds
                                , Card Jack Spades
                                , Card Jack Hearts
                                , Card Six Hearts
                                ])
  print ("jacks over queens"
        , bestHand $ S.fromList [ Card Jack Clubs
                                , Card Jack Diamonds
                                , Card Jack Spades
                                , Card Queen Hearts
                                , Card Queen Spades
                                ])
  print ("queen flush"
        , bestHand $ S.fromList [ Card Two Clubs
                                , Card Three Clubs
                                , Card Four Clubs
                                , Card Five Clubs
                                , Card Queen Clubs
                                ])
  print ("queen straight"
        , bestHand $ S.fromList [ Card Eight Hearts
                                , Card Nine Hearts
                                , Card Ten Hearts
                                , Card Jack Hearts
                                , Card Queen Clubs
                                ])
  print ( "3x jacks"
        , bestHand $ S.fromList [ Card Eight Hearts
                                , Card Nine Hearts
                                , Card Jack Clubs
                                , Card Jack Diamonds
                                , Card Jack Hearts
                                ])
  print ("2x jack, 2x ten"
        , bestHand $ S.fromList [ Card Eight Hearts
                                , Card Ten Hearts
                                , Card Ten Clubs
                                , Card Jack Diamonds
                                , Card Jack Hearts
                                ])
  print ("2x jack"
        , bestHand $ S.fromList [ Card Six Hearts
                                , Card Eight Hearts
                                , Card Ten Clubs
                                , Card Jack Diamonds
                                , Card Jack Hearts
                                ])
  print ("ace high"
        , bestHand $ S.fromList [ Card Six Hearts
                                , Card Eight Hearts
                                , Card Ten Clubs
                                , Card Jack Diamonds
                                , Card Ace Hearts
                                ])
