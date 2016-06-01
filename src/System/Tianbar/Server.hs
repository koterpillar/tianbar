module System.Tianbar.Server (
    Server,
    handleURI,
    startServer,
    stopServer,
) where

-- Server to handle JS callbacks

import Control.Concurrent
import Control.Monad

import Data.Tuple

import GI.WebKit2.Objects.WebView

import System.Tianbar.Callbacks
import System.Tianbar.Plugin
import System.Tianbar.Plugin.All

newtype Server = Server { serverPlugins :: MVar AllPlugins }

startServer :: WebView -> IO Server
startServer wk = do
    plugins <- initialize (callbacks wk)
    pluginsVar <- newMVar plugins
    return $ Server pluginsVar

handleURI ::  Server -> URI -> IO (Maybe Response)
handleURI server uri = modifyMVar (serverPlugins server) $
    liftM swap . flip runPlugin uri

stopServer :: Server -> IO ()
stopServer server = takeMVar (serverPlugins server) >>= destroy
