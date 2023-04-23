module GameLogic where

import Cards


data Player = Player { name :: String, hand :: [Card] }
  deriving (Eq, Show)

data GameState = GameState { players :: [Player], deck :: [Card], discardPile :: [Card] }
  deriving (Show)

data Operator = Plus | Minus

type Move = [(Operator, Card)]


newGame :: (String, String) -> IO GameState
newGame names = do
  shuffledDeck <- shuffleDeck createDeck
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
    countTotal topCard' [] = fromEnum (rank topCard') + 1
    countTotal topCard' ((operator, card):xs) = case operator of
      Plus -> countTotal topCard' xs + fromEnum (rank card) + 1
      Minus -> countTotal topCard' xs - fromEnum (rank card) + 1

playCard :: Player -> Card -> GameState -> GameState
playCard player card state = state { players = map (\p -> if p == player then p {hand = filter (/= card) (hand p)} else p) (players state), discardPile = card : discardPile state }