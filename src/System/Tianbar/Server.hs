module System.Tianbar.Server (
    Server (..),
    startServer
) where

-- Server to handle JS callbacks

import Control.Concurrent
import Control.Monad

import Happstack.Server

import Network.Socket
import Network.URI

import System.Tianbar.Callbacks
import System.Tianbar.DBus
-- import System.Tianbar.Socket
import System.Tianbar.Plugin.Basic

data Server = Server { serverOverrideURI :: URI -> URI
                     , serverThread :: ThreadId
                     }

startServer :: Callbacks c => c -> IO Server
startServer c = do
    sock <- bindIPv4 "127.0.0.1" $ fromIntegral aNY_PORT
    thread <- forkIO $ runServer c sock
    portNum <- socketPort sock
    return $ Server (handleURI portNum) thread

runServer :: Callbacks c => c -> Socket -> IO ()
runServer c sock = do
    portNum <- socketPort sock
    let conf = nullConf { port = fromIntegral portNum, logAccess = Just spy }
    dbusPlugin <- dbus c
    simpleHTTPWithSocket sock conf $ msum [ mzero
                                          , dataDirectory
                                          , dbusPlugin
                                          ]

handleURI :: PortNumber -> URI -> URI
handleURI portNum uri | uriScheme uri == "tianbar:" = uri'
                      | otherwise = uri
    where uri' = uri { uriScheme = "http:"
                     , uriAuthority = Just URIAuth { uriUserInfo = ""
                                                   , uriRegName = "localhost"
                                                   , uriPort = ':' : show portNum
                                                   }
                     }

spy :: String -> String -> t -> String -> Int -> Integer -> String -> String -> IO ()
spy _ _ _ rl _ _ _ _ = putStrLn rl
