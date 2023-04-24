module GameLogic where

import Cards
import System.Random (randomIO)
import Data.List ((\\))
import Control.Monad (guard)

data Player = Player { name :: String, hand :: [Card] }
  deriving (Eq, Show)

data GameState = GameState { players :: [Player], deck :: [Card], discardPile :: [Card] }
  deriving (Show, Eq)

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


isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf [] _ = True
isSubsetOf (x:xs) ys = x `elem` ys && isSubsetOf xs ys


isValidMove :: Move -> Int -> GameState -> Either String ()
isValidMove move player state = do
  let topCard = last (discardPile state)
  let total = countTotal topCard move
  let playerCards = hand (players state !! player)

  if not (isSubsetOf (map snd move) playerCards) then
    Left "Invalid move: Player does not have specified cards."
  else
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


playMove :: Move -> Int -> GameState -> Either String GameState
playMove move player state = do
  case isValidMove move player state of
    Right () -> do
      let cards = map snd move
      let player' = (players state !! player) { hand = hand (players state !! player) \\ cards }
      let discardPile' = discardPile state ++ cards
      let players' = take player (players state) ++ [player'] ++ drop (player + 1) (players state)
      let gameState = state { players = players', discardPile = discardPile' }
      return gameState

    Left error -> Left error