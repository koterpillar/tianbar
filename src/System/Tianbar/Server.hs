module System.Tianbar.Server (
    Server (..),
    startServer
) where

-- Server to handle JS callbacks

import Network.URI

import System.Tianbar.Callbacks
import System.Tianbar.Plugin
import System.Tianbar.Plugin.Combined
import System.Tianbar.Plugin.DataDirectory
import System.Tianbar.Plugin.DBus
import System.Tianbar.Plugin.GSettings
import System.Tianbar.Plugin.Socket

data Server = Server { handleURI :: URI -> IO (Maybe Response)
                     , stopServer :: IO ()
                     }

startServer :: Callbacks -> IO Server
startServer c = do
    plugins <- initialize c :: IO AllPlugins
    return $ Server (runPlugin plugins) (destroy plugins)

type AllPlugins =
    Combined DataDirectory (
        Combined DBusPlugin (
            Combined GSettings (
                Combined SocketPlugin (
                    Combined Empty Empty))))
