{-# LANGUAGE FlexibleContexts #-}
module System.Tianbar.Plugin.Socket where

-- Socket connectivity

import Control.Concurrent
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Maybe

import qualified Data.Map as M

import Network.Socket

import System.Tianbar.Callbacks
import System.Tianbar.Plugin

type SocketMap = M.Map String Socket

data SocketPlugin = SocketPlugin { _spHost :: Callbacks
                                 , _spSock :: SocketMap
                                 }

spHost :: Getter SocketPlugin Callbacks
spHost inj (SocketPlugin h m) = flip SocketPlugin m <$> inj h

spSock :: Lens' SocketPlugin SocketMap
spSock inj (SocketPlugin h m) = SocketPlugin h <$> inj m

instance Plugin SocketPlugin where
    initialize c = do
        return $ SocketPlugin c M.empty

    destroy = mapM_ close . M.elems . view spSock

    handler = dir "socket" $ msum [connectHandler, sendHandler, closeHandler]

connectHandler :: Handler SocketPlugin Response
connectHandler = dir "connect" $ do
    nullDir
    callbackIndex <- look "callbackIndex"
    socketPath <- look "path"
    sock <- liftIO $ do
        s <- socket AF_UNIX Stream defaultProtocol
        connect s $ SockAddrUnix socketPath
        return s
    host <- use spHost
    _ <- liftIO $ forkIO $ void $ forever $ do
        response <- recv sock 4096
        callback host callbackIndex [response]
    spSock . at callbackIndex .= Just sock
    okResponse

sendHandler :: Handler SocketPlugin Response
sendHandler = dir "send" $ do
    nullDir
    callbackIndex <- look "callbackIndex"
    sock <- MaybeT $ getSocket callbackIndex
    dataToSend <- look "data"
    -- TODO: resend until done
    _ <- liftIO $ send sock dataToSend
    okResponse

closeHandler :: Handler SocketPlugin Response
closeHandler = dir "close" $ do
    nullDir
    callbackIndex <- look "callbackIndex"
    sock <- getSocket callbackIndex
    case sock of
        Nothing -> return ()
        Just sock' -> do
            liftIO $ close sock'
            spSock . at callbackIndex .= Nothing
    okResponse

getSocket :: MonadState SocketPlugin m => String -> m (Maybe Socket)
getSocket callbackIndex = use $ spSock . at callbackIndex
