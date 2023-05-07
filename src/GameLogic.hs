module GameLogic (dealCards, newGame, gameLoop) where 

import Cards ( Card(rank), Rank(Ten), cardValue, createDeck, shuffleDeck )
import Data.List ((\\), intercalate, nub, partition)
import Data.List.Extra (replace)
import Data.List.Split (chunksOf)
import System.IO (hFlush, stdout)
import Data.Char (toUpper)
import System.Console.ANSI (clearScreen)
import Text.Regex.TDFA ( (=~) )


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

-- A move is a list of tuples where the first element is an operator (+-)
-- and the second element is a card.
type Move = [(Operator, Card)]


-- Creates a new GameState with the provided player names and a seed
-- If the seed is Nothing, the shuffleDeck method will generate a random seed
newGame :: (String, String) -> Maybe Int -> IO GameState
newGame names seed = do 
  let p1 = Player { name = fst names, hand = [] }
      p2 = Player { name = snd names, hand = [] }
  deck' <- shuffleDeck createDeck seed
  let state = GameState { players = [p1, p2], deck = deck', discardPile = [] }
      state' = dealCards 10 state
  return state' {deck = drop 1 (deck state'), discardPile = [head (deck state')] }
  

-- Deals the requested amount of cards to the players
-- If there are not enough cards in the deck, the remaining cards are dealt
-- Will never deal an uneven amount of cards to the players 
dealCards :: Int -> GameState -> GameState
dealCards n state = do
  let count = min (length (deck state) `div` 2) n
      (hand1, deck') = splitAt count (deck state)
      (hand2, deck'') = splitAt count deck'
      p1 = (head (players state)) { hand = hand (head (players state)) ++ hand1 }
      p2 = (players state !! 1) { hand = hand (players state !! 1) ++ hand2 }
  state { players = [p1, p2], deck = deck'' }


-- Checks if a move is valid. A move is valid if:
-- 1. Each card in the list of moves is unique and in the player's hand 
-- 2. The total of the cards is 10
-- 3. If the move is a 10, it must be the only card in the move
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


-- Checks if a move is valid and plays it if it is
-- If the move is a single 10, each player is dealt 3 cards and the move is played.
--
-- Returns the new GameState if the move was valid
-- Returns an error message if the move was invalid
playMove :: Move -> Int -> GameState -> Either String GameState
playMove move player state = do
  if length move == 1 && rank (snd (head move)) == Ten then do
    let card = snd (head move)
        gameState = dealCards 3 state
        player' = (players gameState !! player) { hand = hand (players gameState !! player) \\ [card] }
        players' = take player (players gameState) ++ [player'] ++ drop (player + 1) (players gameState)
        gameState' = gameState {players = players', discardPile = discardPile gameState ++ [card] }
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


-- Uses regex to parse the input string into a list of moves
-- Returns an error message if the input is invalid
parseInput :: String -> Either String Move
parseInput input = do
  -- Input must be in the form of [+-]RS where R is a rank and S is a suit
  let pattern = "([+-][2-9TJQKA][CDHS])"
      groups = chunksOf 3 input 
      (matches, nonMatches) = partition (=~ pattern) groups
  if null nonMatches then
    Right (map (\x -> (read [x !! 0], read [x !! 1, x !! 2])) matches)
  else
    Left $ "Error at: " ++ show (head nonMatches) ++ " | " ++  "Invalid input: Input must be in the form of: [+-]RS, where R is a rank and S is a suit."


-- Creates a pretty string representation of the GameState
prettyState :: GameState -> Int -> String
prettyState state player = do
  let player' = players state !! player
      hand' = hand player'
      topCard = last (discardPile state)
      playerText = name player' ++ "\'s turn."
      topCardText = "Top card: " ++ show topCard ++ "."
      handText = "Your hand: " ++ show hand' ++ "."
  intercalate "\n" [playerText, topCardText, handText]


-- Passes the turn to the next player and deals them a card
-- Is only called if their is at least one card in the deck
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
        -- If the player passes their turn, check if there are any cards left to draw.
        -- If there are no cards, pass the turn and increase the pass counter.
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
