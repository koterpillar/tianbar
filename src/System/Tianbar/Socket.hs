module System.Tianbar.Socket where

-- Socket connectivity

import Control.Exception
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import qualified Data.Map as M
import Data.Maybe

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
socketConnect sp params = handleBlank $ runMaybeT $ do
    callbackIndex <- liftMT $ lookupQueryParam "callbackIndex" params
    socketPath <- liftMT $ lookupQueryParam "path" params
    liftIO $ do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock $ SockAddrUnix socketPath
        let closeSocket = const (close sock) :: IOException -> IO ()
        _ <- forkIO $ handle closeSocket $ forever $ do
            response <- recv sock 4096
            callback (spHost sp) callbackIndex [response]
        modifyMVar_ (spSock sp) $ return . M.insert callbackIndex sock
    returnContent "ok"

socketSend :: SocketPlugin -> URIParams -> IO String
socketSend sp params = handleBlank $ runMaybeT $ do
    callbackIndex <- liftMT $ lookupQueryParam "callbackIndex" params
    sock <- MaybeT $ withSocket sp callbackIndex
    dataToSend <- liftMT $ lookupQueryParam "data" params
    -- TODO: resend until done
    _ <- liftIO $ send sock dataToSend
    returnContent "ok"

socketClose :: SocketPlugin -> URIParams -> IO String
socketClose sp params = handleBlank $ runMaybeT $ do
    callbackIndex <- liftMT $ lookupQueryParam "callbackIndex" params
    sock <- MaybeT $ withSocket sp callbackIndex

    liftIO $ do
        close sock
        modifyMVar_ (spSock sp) $ return . M.delete callbackIndex
    returnContent ""

withSocket :: SocketPlugin -> String -> IO (Maybe Socket)
withSocket sp callbackIndex = withMVar (spSock sp) $ return . M.lookup callbackIndex
