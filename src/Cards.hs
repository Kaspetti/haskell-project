module Cards (createDeck, shuffleDeck, Card(..), Rank(..), Suit(..)) where

import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

data Suit = Spades | Hearts | Clubs | Diamonds
  deriving (Enum, Eq)

instance Show Suit where
  show Spades = "S"
  show Hearts = "H"
  show Clubs = "C"
  show Diamonds = "D"

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Enum, Eq)

instance Show Rank where
  show Ten = "T"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"
  show x = show (fromEnum x + 2)

data Card = Card Rank Suit
  deriving (Eq)

instance Show Card where
  show (Card rank suit) = show rank ++ show suit

createDeck :: [Card]
createDeck = [Card rank suit | rank <- [Two .. Ace], suit <- [Spades .. Diamonds]]

test = Card Ace Hearts

shuffleDeck :: [Card] -> IO [Card]
shuffleDeck deck = do
  gen <- newStdGen
  return $ shuffle' deck (length deck) gen