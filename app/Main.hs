module Main (main) where

import GameLogic
import Text.Regex.TDFA 
import Data.List.Split (chunksOf)


main :: IO ()
main = do
  state <- newGame ("Player 1", "Player 2") Nothing
  gameLoop state 0 "Welcome to Kaspetri!" 0

