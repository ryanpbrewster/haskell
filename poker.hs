-- poker.hs
{-
 - In the card game poker, a hand consists of five cards and are ranked, from
 - lowest to highest, in the following way:
 -
 -     High Card: Highest value card.
 -     One Pair: Two cards of the same value.
 -     Two Pairs: Two different pairs.
 -     Three of a Kind: Three cards of the same value.
 -     Straight: All cards are consecutive values.
 -     Flush: All cards of the same suit.
 -     Full House: Three of a kind and a pair.
 -     Four of a Kind: Four cards of the same value.
 -     Straight Flush: All cards are consecutive values of same suit.
 -     Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
 -
 - The cards are valued in the order:
 - 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.
 -
 - If two players have the same ranked hands then the rank made up of the
 - highest value wins; for example, a pair of eights beats a pair of fives (see
 - example 1 below). But if two ranks tie, for example, both players have
 - a pair of queens, then highest cards in each hand are compared (see example
 - 4 below); if the highest cards tie then the next highest cards are compared,
 - and so on.
 -
 - Consider the following five hands dealt to two players:
 - Hand        Player 1        Player 2        Winner
 - 1       5H 5C 6S 7S KD   2C 3S 8S 8D TD    Player 2
 - 2       5D 8C 9S JS AC   2C 5C 7D 8S QH    Player 1
 - 3       2D 9C AS AH AC   3D 6D 7D TD QD    Player 2
 - 4       4D 6S 9H QH QC   3D 6D 7H QD QS    Player 1
 - 5       2H 2D 4C 4D 4S   3C 3D 3S 9S 9D    Player 1
 -}

import Data.Map (fromList, (!))
import Data.List (sort, groupBy)
import Data.Maybe (fromJust)

solveProblem txt = let lns = [ map parseCard $ words ln | ln <- lines txt ]
                       inputs = [ (take 5 ln, drop 5 ln) | ln <- lns ]
                       anss = [ (classifyHand h1) > (classifyHand h2) | (h1,h2) <- inputs ]
                   in length $ filter id anss

data Card = Card { getRank :: Int, getSuit :: Int }
instance Show Card where
    show (Card r s) = [rank_revmap ! r, suit_revmap ! s]
instance Ord Card where
    (Card r _) `compare` (Card r' _) = r `compare` r' -- suit is irrelevant
instance Eq Card where
    (Card r s) == (Card r' s') = r == r' && s == s'

rank_map    = fromList $ zip "23456789TJQKA" [0..]
rank_revmap = fromList $ zip [0..] "23456789TJQKA"
suit_map    = fromList $ zip "CDHS" [0..]
suit_revmap = fromList $ zip [0..] "CDHS"

parseCard [r,s] = Card (rank_map ! r) (suit_map ! s)

same f a b = (f a) == (f b)

data HandType = HighCard      [Card] -- descending order
              | OnePair       [Card] -- pair first
              | TwoPair       [Card] -- pairs first
              | Trip          [Card] -- trip first
              | Straight      [Card] -- descending order
              | Flush         [Card] -- descending order
              | FullHouse     [Card] -- trip, then pair
              | Quad          [Card] -- quad first
              | StraightFlush [Card] -- descending order
              | RoyalFlush    [Card] -- descending order
              deriving (Show, Ord, Eq)

classifyHand :: [Card] -> HandType
classifyHand hand =
     let by_rank    = reverse $ sort hand
         (hi,lo)    = (head by_rank, last by_rank)
         globs      = reverse $ sort [(length g,g) | g <- groupBy (same getRank) by_rank]
         rank_count = map fst globs
         by_count   = concat $ map snd globs
         flush          = length (head $ groupBy (same getSuit) hand) == 5
         straight       = length rank_count == 5 && getRank hi - getRank lo == 4
         straight_flush = straight && flush
         royal_flush    = straight_flush && getRank lo == rank_map ! 'T'
         quad           = rank_count == [4,1]
         full_house     = rank_count == [3,2]
         two_pair       = rank_count == [2,2,1]
         trip           = rank_count == [3,1,1]
         one_pair       = rank_count == [2,1,1,1]
     in fromJust $ lookup True [(royal_flush,    RoyalFlush    by_rank)
                               ,(straight_flush, StraightFlush by_rank)
                               ,(quad,           Quad          by_count)
                               ,(full_house,     FullHouse     by_count)
                               ,(flush,          Flush         by_rank)
                               ,(straight,       Straight      by_rank)
                               ,(trip,           Trip          by_count)
                               ,(two_pair,       TwoPair       by_count)
                               ,(one_pair,       OnePair       by_count)
                               ,(True,           HighCard      by_rank)
                               ]
