{-# LANGUAGE FlexibleContexts #-}
module System.Tianbar.Plugin.Socket where

-- Socket connectivity

import Control.Concurrent
import Control.Lens hiding (index)
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
    initialize c = return $ SocketPlugin c M.empty

    destroy = mapM_ close . M.elems . view spSock

    handler = dir "socket" $ msum [connectHandler, sendHandler, closeHandler]

connectHandler :: ServerPart SocketPlugin Response
connectHandler = dir "connect" $ do
    nullDir
    index <- look "callbackIndex"
    socketPath <- look "path"
    sock <- liftIO $ do
        s <- socket AF_UNIX Stream defaultProtocol
        connect s $ SockAddrUnix socketPath
        return s
    host <- use spHost
    _ <- liftIO $ forkIO $ void $ forever $ do
        response <- recv sock 4096
        callback host index [response]
    spSock . at index .= Just sock
    callbackResponse index

sendHandler :: ServerPart SocketPlugin Response
sendHandler = dir "send" $ do
    nullDir
    index <- look "callbackIndex"
    sock <- MaybeT $ getSocket index
    dataToSend <- look "data"
    -- TODO: resend until done
    _ <- liftIO $ send sock dataToSend
    callbackResponse index

closeHandler :: ServerPart SocketPlugin Response
closeHandler = dir "close" $ do
    nullDir
    index <- look "callbackIndex"
    sock <- getSocket index
    case sock of
        Nothing -> return ()
        Just sock' -> do
            liftIO $ close sock'
            spSock . at index .= Nothing
    callbackResponse index

getSocket :: MonadState SocketPlugin m => String -> m (Maybe Socket)
getSocket callbackIndex = use $ spSock . at callbackIndex
