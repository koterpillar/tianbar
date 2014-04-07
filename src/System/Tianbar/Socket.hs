module System.Tianbar.Socket where

-- Socket connectivity

import Control.Exception
import Control.Concurrent
import Control.Monad

import qualified Data.Map as M

import Graphics.UI.Gtk.WebKit.WebView

import Network.Socket
import Network.URI

import System.Tianbar.Plugin

data SocketPlugin = SocketPlugin { spHost :: WebView
                                 , spSock :: MVar (M.Map String Socket)
                                 }

instance Plugin SocketPlugin where
    initialize wk = do
        socks <- newMVar M.empty
        return $ SocketPlugin wk socks

    destroy sp = withMVar (spSock sp) $ mapM_ close . M.elems

    handleRequest sp = withScheme "socket:" $ \uri -> do
        let params = parseQuery uri
        case uriPath uri of
            "connect" -> socketConnect sp params
            "send" -> socketSend sp params
            "close" -> socketClose sp params
            _ -> returnContent ""

socketConnect :: SocketPlugin -> URIParams -> IO String
socketConnect sp params = do
    let Just callbackIndex = lookupQueryParam "callbackIndex" params
    let Just path = lookupQueryParam "path" params
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock $ SockAddrUnix path
    -- TODO: replace with ByteStrings
    let closeSocket = const (close sock) :: IOException -> IO ()
    forkIO $ handle closeSocket $ forever $ do
        response <- recv sock 4096
        callback (spHost sp) callbackIndex [response]
    modifyMVar_ (spSock sp) $ return . M.insert callbackIndex sock
    returnContent ""

socketSend :: SocketPlugin -> URIParams -> IO String
socketSend sp params = do
    let Just callbackIndex = lookupQueryParam "callbackIndex" params
    sock <- withSocket sp callbackIndex
    case sock of
        Nothing -> returnContent ""
        Just sock' -> do
            let Just dataToSend = lookupQueryParam "data" params
            send sock' dataToSend
            returnContent ""

socketClose :: SocketPlugin -> URIParams -> IO String
socketClose sp params = do
    let Just callbackIndex = lookupQueryParam "callbackIndex" params
    Just sock <- withSocket sp callbackIndex

    close sock
    modifyMVar_ (spSock sp) $ return . M.delete callbackIndex
    returnContent ""

withSocket :: SocketPlugin -> String -> IO (Maybe Socket)
withSocket sp callbackIndex = withMVar (spSock sp) $ return . M.lookup callbackIndex
