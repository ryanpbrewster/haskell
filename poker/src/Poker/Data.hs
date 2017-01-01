module Poker.Data
( Rank(..)
, Suit(..)
, Card(..)
, Hand(..)
, allRanks
, allSuits
, allCards
) where

data Rank = Two
          | Three
          | Four
          | Five
          | Six
          | Seven
          | Eight
          | Nine
          | Ten
          | Jack
          | Queen
          | King
          | Ace
          deriving (Eq, Ord, Show)

allRanks = [Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King, Ace]

rankToChar :: Rank -> Char
rankToChar Two   = '2'
rankToChar Three = '3'
rankToChar Four  = '4'
rankToChar Five  = '5'
rankToChar Six   = '6'
rankToChar Seven = '7'
rankToChar Eight = '8'
rankToChar Nine  = '9'
rankToChar Ten   = 'T'
rankToChar Jack  = 'J'
rankToChar Queen = 'Q'
rankToChar King  = 'K'
rankToChar Ace   = 'A'

data Suit = Clubs
          | Diamonds
          | Hearts
          | Spades
          deriving (Eq, Ord, Show)

allSuits = [Clubs, Diamonds, Hearts, Spades]

allCards = [ Card rank suit | suit <- allSuits, rank <- allRanks ]

suitToChar Clubs    = 'C'
suitToChar Diamonds = 'D'
suitToChar Hearts   = 'H'
suitToChar Spades   = 'S'

data Card = Card Rank Suit
          deriving (Eq, Ord)

instance Show Card where
  show (Card rank suit) = [rankToChar rank, suitToChar suit]


data Hand = HighCard Rank
          | OnePair Rank
          | TwoPair Rank Rank
          | Triple Rank
          | Straight Rank
          | Flush Rank
          | FullHouse Rank Rank
          | Quartet Rank
          | StraightFlush Rank
          deriving (Eq, Ord, Show)
