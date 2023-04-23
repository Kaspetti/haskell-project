module GameLogicSpec (spec) where

import Test.Hspec

import GameLogic
import Cards
import Data.List (nub)

spec :: Spec
spec = do
  describe "newGame" $ do
    it "should create a new game with the given players" $ do
      gameState <- newGame ("player1", "player2")
      length (players gameState) `shouldBe` 2
      name (head (players gameState)) `shouldBe` "player1"
      name (players gameState !! 1) `shouldBe` "player2"
      length (deck gameState) `shouldBe` 52
      length (nub (deck gameState)) `shouldBe` 52
  describe "dealCards" $ do
    it "should deal 10 cards to each player" $ do
      gameState <- newGame ("player1", "player2")
      let gameState' = dealCards 10 gameState
      let hand1 = hand (head (players gameState'))
      let hand2 = hand (players gameState' !! 1)
      let deck' = deck gameState'
      length hand1 `shouldBe` 10
      length hand2 `shouldBe` 10
      length deck' `shouldBe` 32
      hand1 `shouldNotBe` hand2
  describe "shuffleDeck" $ do
    it "should shuffle the deck. Each shuffle should be different" $ do
      deck1 <- shuffleDeck createDeck
      deck2 <- shuffleDeck createDeck
      deck1 `shouldNotBe` deck2
  describe "isValidMove" $ do
    it "should check if a move is valid" $ do
      --Setup
      gameState <- newGame ("player1", "player2")
      let discardPile = [Card Ace Hearts]
      let gameState' = gameState {discardPile = discardPile }
      --Test 1
      let move = [(Plus, Card Two Hearts), (Plus, Card Seven Hearts)]
      let error = isValidMove move gameState'
      error `shouldBe` Right ()
      --Test 2
      let move = [(Plus, Card Two Hearts), (Plus, Card Eight Hearts)]
      let error = isValidMove move gameState'
      error `shouldBe` Left "Invalid move: 11"