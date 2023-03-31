module GameLogic where

import Cards

data Player = Player { name :: String, hand :: [Card] }
data GameState = GameState { players :: [Player], deck :: [Card], discardPile :: [Card] }