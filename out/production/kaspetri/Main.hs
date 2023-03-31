module Main (main) where

import AsciiCreator
import Server
import Client
import Cards
import GameLogic

main :: IO ()
main = do
  gameState <- newGame ["player1", "player2"]
  let gameState' = dealCards gameState
  print gameState'

main1 :: IO ()
main1 = do
  putStrLn "Start server with 'server', start client with 'client'"
  input <- getLine
  case input of
    "server" -> startServer "127.0.0.1" "4242"
    "client" -> startClient "127.0.0.1" "4242"
    _ -> putStrLn "Invalid input"
