module Cards (createDeck, shuffleDeck, cardValue, Card(..), Rank(..), Suit(..)) where

import System.Random (newStdGen)
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


shuffleDeck :: [Card] -> IO [Card]
shuffleDeck deck = do
  gen <- newStdGen
  return $ shuffle' deck (length deck) gen