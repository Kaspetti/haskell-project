module Cards (createDeck, shuffleDeck, cardValue, Card(..), Rank(..), Suit(..)) where

import System.Random (mkStdGen)
import System.Random.Shuffle (shuffle')

data Suit = Spades | Hearts | Clubs | Diamonds
  deriving (Enum, Eq)

instance Show Suit where
  show Spades = "S"
  show Hearts = "H"
  show Clubs = "C"
  show Diamonds = "D"

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King
  deriving (Enum, Eq)

instance Show Rank where
  show Ten = "T"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"
  show x = show (fromEnum x + 1)

data Card = Card {rank :: Rank, suit :: Suit}
  deriving (Eq)

instance Show Card where
  show (Card rank suit) = show rank ++ show suit


cardValue :: Card -> Int
cardValue (Card rank _) = fromEnum rank + 1


createDeck :: [Card]
createDeck = [Card rank suit | rank <- [Ace .. King], suit <- [Spades .. Diamonds]]


shuffleDeck :: [Card] -> Int -> [Card]
shuffleDeck deck seed = do
  let gen = mkStdGen seed
    in shuffle' deck (length deck) gen