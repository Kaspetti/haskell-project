module GameLogicSpec (spec) where

import Test.Hspec

import GameLogic
import Cards
import Data.List (nub)

spec :: Spec
spec = do
  describe "newGame" $ do
    it "should create a new game with the given players" $ do
      gameState <- newGame ("player1", "player2") (Just 0)
      length (players gameState) `shouldBe` 2
      name (head (players gameState)) `shouldBe` "player1"
      name (players gameState !! 1) `shouldBe` "player2"
      length (deck gameState) `shouldBe` 52
      length (nub (deck gameState)) `shouldBe` 52

  describe "dealCards" $ do
    it "should deal 10 cards to each player" $ do
      gameState <- newGame ("player1", "player2") (Just 0)
      let gameState' = dealCards 10 gameState
      let hand1 = hand (head (players gameState'))
      let hand2 = hand (players gameState' !! 1)
      let deck' = deck gameState'
      length hand1 `shouldBe` 10
      length hand2 `shouldBe` 10
      length deck' `shouldBe` 31
      length (discardPile gameState') `shouldBe` 1
      hand1 `shouldNotBe` hand2

  describe "shuffleDeck" $ do
    it "should shuffle the deck. Each shuffle should be different" $ do
      let deck1 = shuffleDeck createDeck 0
      let deck2 = shuffleDeck createDeck 1
      deck1 `shouldNotBe` deck2

  describe "isValidMove" $ do
    it "should check if a move is valid" $ do
      --Setup
      gameState <- newGame ("player1", "player2") (Just 0)
      let gameState' = dealCards 10 gameState
      --Test 1
      let move = [(Plus, Card Eight Hearts), (Minus, Card Three Spades)]
      let error = isValidMove move 0 gameState'
      error `shouldBe` Right ()
      --Test 2
      let move = [(Plus, Card Queen Clubs), (Minus, Card Six Diamonds)]
      let error = isValidMove move 0 gameState'
      error `shouldBe` Left "Invalid move: 11"
      --Test 3
      let move = [(Plus, Card Five Diamonds)]
      let error = isValidMove move 0 gameState'
      error `shouldBe` Left "Invalid move: Player does not have specified cards."
      --Test 4
      let move = [(Plus, Card Ten Spades), (Minus, Card Four Spades)]
      let error = isValidMove move 0 gameState'
      error `shouldBe` Left "Invalid move: Cannot use 10s with other cards."
      --Test 5
      let move = [(Plus, Card Ten Spades), (Plus, Card Ten Spades)]
      let error = isValidMove move 0 gameState'
      error `shouldBe` Left "Invalid move: Cannot use the same card twice."

  describe "playMove" $ do
    it "checks if the move is valid and plays it if it is" $ do
      --Setup
      gameState <- newGame ("player1", "player2") (Just 0)
      let gameState' = dealCards 10 gameState
      --Test 1
      let move = [(Plus, Card Eight Hearts), (Minus, Card Three Spades)]
      let p1 = (head (players gameState')) {hand = [Card Seven Spades, Card Seven Clubs, Card Queen Clubs, Card Six Diamonds, Card Nine Spades, Card Ten Spades, Card Queen Diamonds, Card King Clubs]}
      let discardPile' = discardPile gameState'++[Card Eight Hearts, Card Three Spades]
      let gameState'' = gameState' {players = [p1, players gameState' !! 1], discardPile = discardPile'}
      let result = playMove move 0 gameState'
      result `shouldBe` Right gameState''
      --Test 2
      let move = [(Plus, Card Queen Clubs), (Minus, Card Six Diamonds)]
      let result = playMove move 0 gameState'
      result `shouldBe` Left "Invalid move: 11"

  describe "parseInput" $ do
    it "parses the input string into a move" $ do
      --Test 1
      let input = "+AS+AD-3H"
      let result = [(Plus, Card Ace Spades), (Plus, Card Ace Diamonds), (Minus, Card Three Hearts)]
      parseInput input `shouldBe` Right result
      --Test 2
      let input = "++AS "
      let result = "Invalid input: Input must be in the form of: [+-]RS, where R is a rank and S is a suit."
      parseInput input `shouldBe` Left result
      --Test 2
      let input = ""
      let result = []
      parseInput input `shouldBe` Right result