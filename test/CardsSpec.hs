module CardsSpec (spec) where

import Test.Hspec

import Cards
import Data.List (nub)

spec :: Spec
spec = do
  describe "createDeck" $ do
    it "deck has 52 cards" $ do
      length createDeck `shouldBe` 52
    it "deck has unique cards" $ do
      length (nub createDeck) `shouldBe` 52
  describe "shuffleDeck" $ do
    it "shuffled deck has same cards as unshuffled deck" $ do
      let shuffledDeck = shuffleDeck createDeck 0
      shuffledDeck `shouldMatchList` createDeck