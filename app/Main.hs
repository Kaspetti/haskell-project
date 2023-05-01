module Main (main) where

import Server
import Client
import Cards
import GameLogic


main :: IO ()
main = do
  state <- newGame ("Player 1", "Player 2") Nothing
  let state' = dealCards 10 state
  gameLoop state' 0 "Welcome to Kaspetri!"


main1 :: IO ()
main1 = do
  putStrLn "Start server with 'server', start client with 'client'"
  input <- getLine
  case input of
    "server" -> startServer "127.0.0.1" "4242"
    "client" -> startClient "127.0.0.1" "4242"
    _ -> putStrLn "Invalid input"
