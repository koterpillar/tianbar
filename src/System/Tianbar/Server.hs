module System.Tianbar.Server (
    startServer
) where

-- Server to handle JS callbacks

import Control.Concurrent

import Happstack.Server ( bindIPv4
                        , Conf (..)
                        , nullConf
                        , simpleHTTPWithSocket
                        , toResponse
                        , ok
                        )

import Network.Socket

startServer :: IO PortNumber
startServer = do
    sock <- bindIPv4 "127.0.0.1" $ fromIntegral aNY_PORT
    _ <- forkIO $ runServer sock
    socketPort sock

runServer :: Socket -> IO ()
runServer sock = do
    portNum <- socketPort sock
    let conf = nullConf { port = fromIntegral portNum }
    simpleHTTPWithSocket sock conf $ ok "Hello, World"
