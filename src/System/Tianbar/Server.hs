module System.Tianbar.Server (
    Server (..),
    startServer
) where

-- Server to handle JS callbacks

import GI.WebKit2.Objects.WebView

import System.Tianbar.Callbacks
import System.Tianbar.Plugin
import System.Tianbar.Plugin.All

data Server = Server { handleURI :: URI -> IO (Maybe Response)
                     , stopServer :: IO ()
                     }

startServer :: WebView -> IO Server
startServer c = do
    plugins <- initialize (callbacks c) :: IO AllPlugins
    return $ Server (runPlugin plugins) (destroy plugins)
