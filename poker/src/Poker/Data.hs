module Poker.Data
( Rank(..)
, Suit(..)
, Card(..)
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

suitToChar Clubs    = 'C'
suitToChar Diamonds = 'D'
suitToChar Hearts   = 'H'
suitToChar Spades   = 'S'

data Card = Card Rank Suit
          deriving (Eq, Ord)

instance Show Card where
  show (Card rank suit) = [rankToChar rank, suitToChar suit]
