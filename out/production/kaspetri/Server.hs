module Server (startServer) where

import Network.Socket
import Network.Socket.ByteString ( recv, sendAll, sendAllTo)
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent ( forkIO )
import Control.Exception ( bracket, bracketOnError )
import Control.Monad ( unless )
import Data.Binary ( decode, encode, Binary)

startServer :: HostName -> ServiceName -> IO ()
startServer host port = withSocketsDo $ do
  addr <- head <$> getAddrInfo (Just defaultHints) (Just host) (Just port)
  bracket
    (open addr)
    close
    (`runServer` [])
  where
    open addr = bracketOnError
      (openSocket addr)
      close
      (\sock -> do
        setSocketOption sock ReuseAddr 1
        bind sock (addrAddress addr)
        listen sock 2
        putStrLn "Server started. Listening on port 4242"
        return sock)

runServer :: Socket -> [SockAddr] -> IO ()
runServer sock clients = do
  conn@(_, addr) <- accept sock
  _ <- forkIO $ runConn conn BS.empty (addr:clients)
  runServer sock (addr:clients)

runConn :: (Socket, SockAddr) -> BS.ByteString -> [SockAddr] -> IO ()
runConn conn@(sock, addr) client clients= do
  print clients

  if client == BS.empty
    then do
      putStrLn $ "Client connected from " ++ show addr
      sendAllTo sock (BS.pack "Enter your name: ") addr
      response <- recv sock 1024
      let name = BS.strip response
      sendAllTo sock (BS.pack "Welcome to the server " <> name) addr
      runConn conn name clients
  else do
    msg <- recv sock 1024
    unless (BS.null msg) $ do
      mapM_ (sendAllTo sock (client <> BS.pack ": " <> msg)) [x | x <- clients, x /= addr]
    runConn conn client clients