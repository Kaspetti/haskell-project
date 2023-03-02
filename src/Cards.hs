module Cards where

data Suit = Spades | Hearts | Clubs | Diamonds
  deriving (Enum)
data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Enum)
data Card = Card Rank Suit

instance Show Suit where
  show Spades = "S"
  show Hearts = "H"
  show Clubs = "C"
  show Diamonds = "D"

instance Show Rank where
  show Ten = "T"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"
  show x = show (fromEnum x + 2)

instance Show Card where
  show (Card rank suit) = show rank ++ show suit