module GameLogic where

import Cards
import System.Random(randomIO)

data Player = Player { name :: String, hand :: [Card] }
  deriving (Eq, Show)

data GameState = GameState { players :: [Player], deck :: [Card], discardPile :: [Card] }
  deriving (Show)

data Operator = Plus | Minus

type Move = [(Operator, Card)]


newGame :: (String, String) -> Maybe Int -> IO GameState
newGame names seed = do
  let p1 = Player { name = fst names, hand = [] }
  let p2 = Player { name = snd names, hand = [] }

  case seed of
    Just seed' -> return GameState { players = [p1, p2], deck = shuffleDeck createDeck seed', discardPile = [] }
    Nothing -> do
      seed' <- randomIO
      return GameState { players = [p1, p2], deck = shuffleDeck createDeck seed', discardPile = [] }


dealCards :: Int -> GameState -> GameState
dealCards n state = do
  let (hand1, deck') = splitAt n (deck state)
  let (hand2, deck'') = splitAt n deck'
  let p1 = (head (players state)) { hand = hand1 }
  let p2 = (players state !! 1) { hand = hand2 }
  let discardPile' = [head deck'']
  state { players = [p1, p2], deck = tail deck'', discardPile = discardPile' }


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