module System.Tianbar.Plugin.Socket where

-- Socket connectivity

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Map as M

import Network.Socket

import System.Tianbar.Callbacks
import System.Tianbar.Plugin

data SocketPlugin = SocketPlugin { spHost :: Callbacks
                                 , spSock :: MVar (M.Map String Socket)
                                 }

instance Plugin SocketPlugin where
    initialize c = do
        socks <- newMVar M.empty
        return $ SocketPlugin c socks

    destroy sp = withMVar (spSock sp) $ mapM_ close . M.elems

    handler plugin = dir "socket" $ msum $ map (\act -> act plugin) acts
        where acts = [connectHandler, sendHandler, closeHandler]

connectHandler :: SocketPlugin -> Handler Response
connectHandler sp = dir "connect" $ do
    nullDir
    callbackIndex <- look "callbackIndex"
    socketPath <- look "path"
    sock <- liftIO $ do
        s <- socket AF_UNIX Stream defaultProtocol
        connect s $ SockAddrUnix socketPath
        return s
    _ <- liftIO $ forkIO $ void $ forever $ do
        response <- recv sock 4096
        callback (spHost sp) callbackIndex [response]
    liftIO $ modifyMVar_ (spSock sp) $ return . M.insert callbackIndex sock
    stringResponse "ok"

sendHandler :: SocketPlugin -> Handler Response
sendHandler sp = dir "send" $ do
    nullDir
    callbackIndex <- look "callbackIndex"
    -- TODO: Maybe
    Just sock <- withSocket sp callbackIndex
    dataToSend <- look "data"
    -- TODO: resend until done
    _ <- liftIO $ send sock dataToSend
    stringResponse "ok"

closeHandler :: SocketPlugin -> Handler Response
closeHandler sp = dir "close" $ do
    nullDir
    callbackIndex <- look "callbackIndex"
    sock <- withSocket sp callbackIndex
    case sock of
        Nothing -> return ()
        Just sock' -> liftIO $ do
            close sock'
            modifyMVar_ (spSock sp) $ return . M.delete callbackIndex
    stringResponse "ok"

withSocket :: MonadIO m => SocketPlugin -> String -> m (Maybe Socket)
withSocket sp callbackIndex = liftIO $ withMVar (spSock sp) $ return . M.lookup callbackIndex
