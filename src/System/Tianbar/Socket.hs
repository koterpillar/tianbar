module System.Tianbar.Socket where

-- Socket connectivity

import Control.Exception
import Control.Concurrent
import Control.Monad

import Graphics.UI.Gtk.WebKit.WebView

import Network.Socket
import Network.URI

import System.Tianbar.Plugin

data SocketPlugin = SocketPlugin { spHost :: WebView }

instance Plugin SocketPlugin where
    initialize = return . SocketPlugin

    handleRequest (SocketPlugin wk) = withScheme "socket:" $ \uri -> do
        let params = parseQuery uri
        let Just callbackIndex = lookupQueryParam "callbackIndex" params
        let Just socketPath = lookupQueryParam "path" params
        -- TODO: support sending data more than once
        let Just dataToSend = lookupQueryParam "data" params
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock $ SockAddrUnix socketPath
        -- TODO: replace with ByteStrings
        -- TODO: resend data until done
        _ <- send sock dataToSend
        shutdown sock ShutdownSend
        let closeSocket = const (close sock) :: IOException -> IO ()
        forkIO $ flip catch closeSocket $ forever $ do
            response <- recv sock 4096
            callback wk callbackIndex [response]
        returnContent ""
