module Client (startClient) where

import Network.Socket
import Network.Socket.ByteString ( recv, sendAll )
import qualified Data.ByteString.Char8 as BS
import Control.Exception (bracket, bracketOnError)
import Control.Monad (unless)
import Control.Concurrent (forkIO)
import Data.Binary ( decode, encode, Binary )

startClient :: HostName -> ServiceName -> IO ()
startClient host port = withSocketsDo $ do
  addr <- head <$> getAddrInfo (Just defaultHints) (Just host) (Just port)
  bracket
    (open addr)
    close
    runClient
  where
    open addr = bracketOnError
      (openSocket addr)
      close
      (\sock -> do
        connect sock (addrAddress addr)
        putStrLn "Client started. Connected to server."
        return sock)

runClient :: Socket -> IO ()
runClient sock = do
  _ <- forkIO $ clientListener sock
  clientSender sock

clientListener :: Socket -> IO ()
clientListener sock = do
  msg <- recv sock 1024
  unless (BS.null msg) $ do
    BS.putStrLn msg
    clientListener sock

clientSender :: Socket -> IO ()
clientSender sock = do
  msg <- BS.getLine
  unless (BS.null msg) $ do
    sendAll sock msg
    clientSender sock