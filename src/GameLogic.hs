module GameLogic where

import Cards
import System.Random (randomIO)
import Data.List ((\\), intercalate, nub)
import Data.List.Extra (replace)
import Data.List.Split (chunksOf)
import System.IO (hFlush, stdout)
import Data.Char (toUpper)
import Data.Either (partitionEithers)
import System.Console.ANSI (clearScreen)
import Data.Maybe (maybe)


data Player = Player { name :: String, hand :: [Card] }
  deriving (Eq, Show)

data GameState = GameState { players :: [Player], deck :: [Card], discardPile :: [Card] }
  deriving (Show, Eq)

data Operator = Plus | Minus
  deriving (Show, Eq)

instance Read Operator where
  readsPrec _ s = case s of
    "+" -> [(Plus, "")]
    "-" -> [(Minus, "")]
    _ -> []

type Move = [(Operator, Card)]


newGame :: (String, String) -> Maybe Int -> IO GameState
newGame names seed = do
  let p1 = Player { name = fst names, hand = [] }
      p2 = Player { name = snd names, hand = [] }

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
  GameState { players = [p1, p2], deck = tail deck'', discardPile = discardPile' }


isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf [] _ = True
isSubsetOf (x:xs) ys = x `elem` ys && isSubsetOf xs ys


isValidMove :: Move -> Int -> GameState -> Either String ()
isValidMove move player state = do
  let topCard = last (discardPile state)
      total = countTotal topCard move
      playerCards = hand (players state !! player)

  if length (nub (map snd move)) /= length move then
    Left "Invalid move: Cannot use the same card twice."
  else do
    if length move > 1 && foldr (\(_, card) acc -> acc || cardValue card == 10) False move then
      Left "Invalid move: Cannot use 10s with other cards."
    else do
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
          player' = (players state !! player) { hand = hand (players state !! player) \\ cards }
          discardPile' = discardPile state ++ cards
          players' = take player (players state) ++ [player'] ++ drop (player + 1) (players state)
          gameState = state { players = players', discardPile = discardPile' }
      return gameState

    Left error -> Left error


parseInput :: String -> Either String Move
parseInput input = do
  let moves = chunksOf 3 input
  if foldr (\x acc -> acc && length x == 3) True moves then do
    let (errors, moves') = partitionEithers (map parseMove moves)
    if null errors then
      Right moves'
    else
      Left (head errors)
  else
    Left "Invalid input: Input must be in the form of: [+-]RS, where R is a rank and S is a suit."
  where
    parseMove :: String -> Either String (Operator, Card)
    parseMove move = do
      if move !! 0 `notElem` "+-" then
        Left ("Invalid input at [" ++ move ++ "]: First character must be either '+' or '-'.")
      else
        if move !! 1 `notElem` "23456789TJQKA" then
          Left ("Invalid input at [" ++ move ++ "]: Second character must be a rank.")
        else
          if move !! 2 `notElem` "CDHS" then
            Left ("Invalid input at [" ++ move ++ "]: Third character must be a suit.")
          else
            Right (read [move !! 0], read [move !! 1, move !! 2])


prettyState :: GameState -> Int -> String
prettyState state player = do
  let player' = players state !! player
      hand' = hand player'
      topCard = last (discardPile state)
      playerText = name player' ++ "\'s turn."
      topCardText = "Top card: " ++ show topCard ++ "."
      handText = "Your hand: " ++ show hand' ++ "."
  intercalate "\n" [playerText, topCardText, handText]

gameLoop :: GameState -> Int -> String -> IO ()
gameLoop state curPlayer msg = do
  clearScreen
  putStrLn msg
  putStrLn $ prettyState state curPlayer
  putStr ("Enter your move: " ++ show (last (discardPile state))) >> hFlush stdout
  input <- getLine
  let input' = map toUpper (replace " " "" input)
  case parseInput input' of
    Right moves -> do
      undefined
    Left error -> do
      gameLoop state curPlayer error