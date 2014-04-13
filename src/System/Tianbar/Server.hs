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

import System.Random

import System.Tianbar.Callbacks
import System.Tianbar.Plugin
import System.Tianbar.Plugin.Basic
import System.Tianbar.Plugin.Combined
import System.Tianbar.Plugin.DBus
import System.Tianbar.Plugin.Socket

data Server = Server { serverOverrideURI :: URI -> URI
                     , stopServer :: IO ()
                     }

startServer :: Callbacks -> IO Server
startServer c = do
    sock <- bindIPv4 "127.0.0.1" $ fromIntegral aNY_PORT
    prefix <- replicateM 20 $ randomRIO ('a', 'z')
    plugins <- initialize c :: IO AllPlugins
    thread <- forkIO $ runServer sock prefix plugins
    portNum <- socketPort sock
    return $ Server (handleURI portNum prefix) (killServer thread plugins)

type AllPlugins = Combined DBusPlugin (
                  Combined SocketPlugin (
                  Combined DataDirectory (
                  Combined Empty Empty)))

runServer :: Plugin p => Socket -> String -> p -> IO ()
runServer sock prefix plugin = do
    portNum <- socketPort sock
    let conf = nullConf { port = fromIntegral portNum }
    simpleHTTPWithSocket sock conf $ dir prefix $ handler plugin

killServer :: Plugin p => ThreadId -> p -> IO ()
killServer thread p = do
    destroy p
    killThread thread

handleURI :: PortNumber -> String -> URI -> URI
handleURI portNum prefix uri | uriScheme uri == "tianbar:" = uri'
                             | otherwise = uri
    where uri' = uri { uriScheme = "http:"
                     , uriAuthority = Just URIAuth { uriUserInfo = ""
                                                   , uriRegName = "localhost"
                                                   , uriPort = ':' : show portNum
                                                   }
                     , uriPath = "/" ++ prefix ++ uriPath uri
                     }