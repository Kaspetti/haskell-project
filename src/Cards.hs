module Cards (createDeck, shuffleDeck, cardValue, parseCard, Card(..), Rank(..), Suit(..)) where

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

instance Read Card where
  readsPrec _ s = case parseCard s of
    Just card -> [(card, "")]
    Nothing -> []


parseRank :: Char -> Maybe Rank
parseRank 'A' = Just Ace
parseRank '2' = Just Two
parseRank '3' = Just Three
parseRank '4' = Just Four
parseRank '5' = Just Five
parseRank '6' = Just Six
parseRank '7' = Just Seven
parseRank '8' = Just Eight
parseRank '9' = Just Nine
parseRank 'T' = Just Ten
parseRank 'J' = Just Jack
parseRank 'Q' = Just Queen
parseRank 'K' = Just King
parseRank _ = Nothing


parseSuit :: Char -> Maybe Suit
parseSuit 'H' = Just Hearts
parseSuit 'S' = Just Spades
parseSuit 'C' = Just Clubs
parseSuit 'D' = Just Diamonds
parseSuit _ = Nothing


parseCard :: String -> Maybe Card
parseCard [rank, suit] = do
  rank' <- parseRank rank
  suit' <- parseSuit suit
  return (Card rank' suit')
parseCard _ = Nothing


cardValue :: Card -> Int
cardValue (Card rank _) = fromEnum rank + 1


createDeck :: [Card]
createDeck = [Card rank suit | rank <- [Ace .. King], suit <- [Spades .. Diamonds]]


shuffleDeck :: [Card] -> Int -> [Card]
shuffleDeck deck seed = do
  let gen = mkStdGen seed
    in shuffle' deck (length deck) gen
