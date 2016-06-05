module System.Tianbar.Server (
    Server,
    handleURI,
    startServer,
    stopServer,
) where

-- Server to handle JS callbacks

import Control.Concurrent

import Data.Tuple

import GI.WebKit2.Objects.WebView

import System.Tianbar.Plugin
import System.Tianbar.Plugin.All

data Server = Server { serverHost :: WebView
                     , serverPlugins :: MVar AllPlugins }

startServer :: WebView -> IO Server
startServer wk = do
    plugins <- initialize
    pluginsVar <- newMVar plugins
    return $ Server wk pluginsVar

handleURI ::  Server -> URI -> IO (Maybe Response)
handleURI server uri = modifyMVar (serverPlugins server) $
    fmap swap . \p -> runPlugin p uri (serverHost server)

stopServer :: Server -> IO ()
stopServer server = takeMVar (serverPlugins server) >>= destroy
