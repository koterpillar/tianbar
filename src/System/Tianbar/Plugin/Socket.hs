{-# LANGUAGE FlexibleContexts #-}
module System.Tianbar.Plugin.Socket where

-- Socket connectivity

import Control.Concurrent
import Control.Lens hiding (index)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Trans.Maybe

import qualified Data.ByteString.UTF8 as U
import qualified Data.Map as M

import Network.Socket
import qualified Network.Socket.ByteString as SB

import System.Tianbar.Callbacks
import System.Tianbar.Plugin

type SocketMap = M.Map CallbackIndex Socket

data SocketPlugin = SocketPlugin { _spSock :: SocketMap
                                 }

spSock :: Lens' SocketPlugin SocketMap
spSock inj (SocketPlugin m) = SocketPlugin <$> inj m

instance Plugin SocketPlugin where
    initialize = return $ SocketPlugin M.empty

    destroy = mapM_ close . M.elems . view spSock

    handler = dir "socket" $ msum [connectHandler, sendHandler, closeHandler]

connectHandler :: ServerPart SocketPlugin Response
connectHandler = dir "connect" $ do
    nullDir
    socketPath <- look "path"
    sock <- liftIO $ do
        s <- socket AF_UNIX Stream defaultProtocol
        connect s $ SockAddrUnix socketPath
        return s
    (callback, index) <- newCallback
    _ <- liftIO $ forkIO $ void $ forever $ do
        sockData <- SB.recv sock 4096
        liftIO $ callback [U.toString sockData]
        return ()
    spSock . at index .= Just sock
    callbackResponse index

sendHandler :: ServerPart SocketPlugin Response
sendHandler = dir "send" $ do
    nullDir
    index <- fromData
    sock <- MaybeT $ getSocket index
    dataToSend <- lookBS "data"
    -- TODO: resend until done
    _ <- liftIO $ SB.send sock dataToSend
    callbackResponse index

closeHandler :: ServerPart SocketPlugin Response
closeHandler = dir "close" $ do
    nullDir
    index <- fromData
    sock <- getSocket index
    case sock of
        Nothing -> return ()
        Just sock' -> do
            liftIO $ close sock'
            spSock . at index .= Nothing
    callbackResponse index

getSocket :: MonadState SocketPlugin m => CallbackIndex -> m (Maybe Socket)
getSocket callbackIndex = use $ spSock . at callbackIndex
