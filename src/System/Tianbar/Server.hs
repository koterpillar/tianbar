module System.Tianbar.Server (
    Server (..),
    startServer
) where

-- Server to handle JS callbacks

import Control.Concurrent

import Happstack.Server

import Network.Socket
import Network.URI

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
    plugins <- initialize c :: IO AllPlugins
    thread <- forkIO $ runServer sock plugins
    portNum <- socketPort sock
    return $ Server (handleURI portNum) (killServer thread plugins)

type AllPlugins = Combined DBusPlugin (
                  Combined SocketPlugin (
                  Combined DataDirectory (
                  Combined Empty Empty)))

runServer :: Plugin p => Socket -> p -> IO ()
runServer sock p = do
    portNum <- socketPort sock
    let conf = nullConf { port = fromIntegral portNum }
    simpleHTTPWithSocket sock conf $ handler p

killServer :: Plugin p => ThreadId -> p -> IO ()
killServer thread p = do
    destroy p
    killThread thread

handleURI :: PortNumber -> URI -> URI
handleURI portNum uri | uriScheme uri == "tianbar:" = uri'
                      | otherwise = uri
    where uri' = uri { uriScheme = "http:"
                     , uriAuthority = Just URIAuth { uriUserInfo = ""
                                                   , uriRegName = "localhost"
                                                   , uriPort = ':' : show portNum
                                                   }
                     }
