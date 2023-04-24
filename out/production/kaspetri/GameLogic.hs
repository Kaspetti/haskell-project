module GameLogic where

import Cards
import System.Random(randomIO)

data Player = Player { name :: String, hand :: [Card] }
  deriving (Eq, Show)

data GameState = GameState { players :: [Player], deck :: [Card], discardPile :: [Card] }
  deriving (Show)

data Operator = Plus | Minus

type Move = [(Operator, Card)]


newGame :: (String, String) -> IO GameState
newGame names = do
  seed <- randomIO :: IO Int
  let shuffledDeck = shuffleDeck createDeck seed
  let p1 = Player { name = fst names, hand = [] }
  let p2 = Player { name = snd names, hand = [] }
  return GameState { players = [p1, p2], deck = shuffledDeck, discardPile = [] }


dealCards :: Int -> GameState -> GameState
dealCards n state = do
  let (hand1, deck') = splitAt n (deck state)
  let (hand2, deck'') = splitAt n deck'
  let p1 = (head (players state)) { hand = hand1 }
  let p2 = (players state !! 1) { hand = hand2 }
  state { players = [p1, p2], deck = deck'' }


isValidMove :: Move -> GameState -> Either String ()
isValidMove move state = do
  let topCard = head (discardPile state)
  let total = countTotal topCard move
  if total == 10 then
    return ()
  else
    Left ("Invalid move: " ++ show total)
  where
    countTotal :: Card -> Move -> Int
    countTotal topCard' [] = cardValue topCard'
    countTotal topCard' ((operator, card):xs) = case operator of
      Plus -> countTotal topCard' xs + cardValue card
      Minus -> countTotal topCard' xs - cardValue card


playMove :: Move -> Int -> GameState -> GameState
playMove move player state = undefined