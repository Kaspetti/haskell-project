module Main (main) where

import GameLogic
import System.Console.ANSI (clearScreen)


main :: IO ()
main = do
  clearScreen
  putStrLn "Player 1, enter your name:"
  player1 <- getLine
  putStrLn "Player 2, enter your name:"
  player2 <- getLine
  state <- newGame (player1, player2) Nothing
  gameLoop state 0 "Welcome to Kaspetri!" 0

