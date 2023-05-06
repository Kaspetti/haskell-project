module GameLogic (dealCards, newGame, gameLoop) where 

import Cards
import System.Random (randomIO)
import Data.List ((\\), intercalate, nub)
import Data.List.Extra (replace)
import Data.List.Split (chunksOf)
import System.IO (hFlush, stdout)
import Data.Char (toUpper)
import Data.Either (partitionEithers)
import System.Console.ANSI (clearScreen)


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
  deck' <- shuffleDeck createDeck seed
  return GameState { players = [p1, p2], deck = deck', discardPile = [] } 
  

dealCards :: Int -> GameState -> GameState
dealCards n state = do
  let (hand1, deck') = splitAt n (deck state)
  let (hand2, deck'') = splitAt n deck'
  let p1 = (head (players state)) { hand = hand (head (players state)) ++ hand1 }
  let p2 = (players state !! 1) { hand = hand (players state !! 1) ++ hand2 }
  let discardPile' = [head deck'']
  GameState { players = [p1, p2], deck = tail deck'', discardPile = discardPile' }


isValidMove :: Move -> Int -> GameState -> Either String ()
isValidMove move player state = do
  let topCard = last (discardPile state)
      total = countTotal topCard move
      playerCards = hand (players state !! player)
      cardsNotInHand = map snd move \\ playerCards

  if length (nub (map snd move)) /= length move then
    Left "Invalid move: Cannot use the same card twice."
  else do
    if length move > 1 && foldr (\(_, card) acc -> acc || cardValue card == 10) False move then
      Left "Invalid move: Cannot use 10s with other cards."
    else do
      if not (null cardsNotInHand) then
        Left ("Invalid move: Player does not have specified cards: " ++ intercalate ", " (map show cardsNotInHand))
      else
        if total == 10 then
          return ()
        else
          Left ("Invalid move: Total was " ++ show total)
        where
          countTotal :: Card -> Move -> Int
          countTotal topCard' [] = cardValue topCard'
          countTotal topCard' ((operator, card):xs) = case operator of
            Plus -> countTotal topCard' xs + cardValue card
            Minus -> countTotal topCard' xs - cardValue card


playMove :: Move -> Int -> GameState -> Either String GameState
playMove move player state = do
  if length move == 1 && rank (snd (head move)) == Ten then do
    --TODO: Check if there are 6 cards left in the deck
    let card = snd (head move)
        gameState = dealCards 3 state
        gameState' = gameState { discardPile = discardPile gameState ++ [card] }
    return gameState'
  else do
    case isValidMove move player state of
      Right () -> do 
        let cards = map snd move
            player' = (players state !! player) { hand = hand (players state !! player) \\ cards }
            discardPile' = discardPile state ++ cards
            players' = take player (players state) ++ [player'] ++ drop (player + 1) (players state)
            gameState = state { players = players', discardPile = discardPile' }
        return gameState

      Left error' -> Left error'


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


passTurn :: GameState -> Int -> GameState
passTurn state player = do
  let player' = (players state !! player) { hand = hand (players state !! player) ++ [head (deck state)] }
      players' = take player (players state) ++ [player'] ++ drop (player + 1) (players state)
  state { players = players', deck = tail (deck state) }


winCondition :: GameState -> Int -> Bool
winCondition state player = do
  let player' = players state !! player
  null (hand player')


gameLoop :: GameState -> Int -> String -> Int -> IO ()
gameLoop state curPlayer msg passCounter
  -- End the game with a draw if both players have passed their turn 3 times.
  | passCounter == 6 = do
      putStrLn "Game over. Draw: Both players passed their turn 3 times."
  | otherwise = do
      clearScreen  
      putStrLn msg
      putStrLn $ prettyState state curPlayer
      putStr ("Enter your move: " ++ show (last (discardPile state))) >> hFlush stdout
      input <- getLine
      let input' = map toUpper (replace " " "" input)

      if input' == "" then do
        if null (deck state) && length (discardPile state) == 1 then do
          let msg' = name (players state !! curPlayer) ++ " passed their turn but there are no cards to draw."
          gameLoop state ((curPlayer + 1) `mod` 2) msg' (passCounter + 1)
        else do
          --Shuffle the deck before drawing if the deck is empty.
          if null (deck state) then do
            deck' <- shuffleDeck (init (discardPile state)) Nothing
            let discardPile' = [last (discardPile state)]
                state' = passTurn (state { deck = deck', discardPile = discardPile' }) curPlayer
                msg' = name (players state' !! curPlayer) ++ " passed their turn."
            gameLoop state' ((curPlayer + 1) `mod` 2) msg' 0
          else do
            let state' = passTurn state curPlayer
                msg' = name (players state' !! curPlayer) ++ " passed their turn."
            gameLoop state' ((curPlayer + 1) `mod` 2) msg' 0
      else do
        case parseInput input' of
          Right moves -> do
            let play = playMove moves curPlayer state
            case play of
              Right state' -> do
                if winCondition state' curPlayer then do
                  putStrLn $ name (players state' !! curPlayer) ++ " won the game!"
                else do
                  let msg' = name (players state !! curPlayer) ++ " played " ++ show (last (discardPile state)) ++ input' ++ "."
                  gameLoop state' ((curPlayer + 1) `mod` 2) msg' 0
              Left error' -> do
                gameLoop state curPlayer error' 0
          Left error' -> do
            gameLoop state curPlayer error' 0 
