module System.Tianbar.Server (
    Server,
    handleURI,
    startServer,
    stopServer,
) where

-- Server to handle JS callbacks

import Control.Concurrent

import GI.WebKit2.Objects.WebView

import System.Tianbar.Callbacks
import System.Tianbar.Plugin
import System.Tianbar.Plugin.All

newtype Server = Server { serverState :: MVar (Callbacks, AllPlugins) }

startServer :: WebView -> IO Server
startServer wk = do
    plugins <- initialize
    pluginsVar <- newMVar (callbacks wk, plugins)
    return $ Server pluginsVar

handleURI ::  Server -> URI -> IO (Maybe Response)
handleURI server uri = modifyMVar (serverState server) $ \(cb, plugins) -> do
    ((resp, plugins'), cb') <- runPlugin plugins uri cb
    return ((cb', plugins'), resp)

stopServer :: Server -> IO ()
stopServer server = do
    (_, plugins) <- takeMVar $ serverState server
    destroy plugins
