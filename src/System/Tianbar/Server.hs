module System.Tianbar.Server (
    startServer
) where

-- Server to handle JS callbacks

import Control.Concurrent
import Control.Monad

import Happstack.Server

import Network.Socket

import System.Tianbar.DBus
import System.Tianbar.Socket
import System.Tianbar.Plugin.Basic

startServer :: IO PortNumber
startServer = do
    sock <- bindIPv4 "127.0.0.1" $ fromIntegral aNY_PORT
    _ <- forkIO $ runServer sock
    socketPort sock

runServer :: Socket -> IO ()
runServer sock = do
    portNum <- socketPort sock
    let conf = nullConf { port = fromIntegral portNum, logAccess = Just spy }
    simpleHTTPWithSocket sock conf $ msum [ mzero
                                          , dataDirectory
                                          ]

spy :: String -> String -> t -> String -> Int -> Integer -> String -> String -> IO ()
spy _ _ _ rl _ _ _ _ = putStrLn rl
