module GameLogic where

import Cards

data Player = Player { name :: String, hand :: [Card] }
  deriving (Eq, Show)

data GameState = GameState { players :: [Player], deck :: [Card], discardPile :: [Card] }
  deriving (Show)

newGame :: [String] -> IO GameState
newGame names = do
  shuffledDeck <- shuffleDeck createDeck
  return GameState { players = map (\n -> Player { name = n, hand = [] }) names, deck = shuffledDeck, discardPile = [] }

dealCards :: GameState -> GameState
dealCards state = state { players = map (\p -> p { hand = take 10 (deck state) }) (players state), deck = drop 10 (deck state) }

playCard :: Player -> Card -> GameState -> GameState
playCard player card state = state { players = map (\p -> if p == player then p {hand = filter (/= card) (hand p)} else p) (players state), discardPile = card : discardPile state }