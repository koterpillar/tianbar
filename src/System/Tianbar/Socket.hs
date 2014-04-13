module System.Tianbar.Socket where

-- Socket connectivity

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import qualified Data.Map as M

import Happstack.Server

import Network.Socket

import System.Tianbar.Callbacks

data SocketPlugin c = SocketPlugin { spHost :: c
                                   , spSock :: MVar (M.Map String Socket)
                                   }

socketPlugin :: Callbacks c => c -> IO (ServerPartT IO Response)
socketPlugin c = do
    socks <- newMVar M.empty
    return $ socketHandler $ SocketPlugin c socks

destroy :: SocketPlugin c -> IO ()
destroy sp = withMVar (spSock sp) $ mapM_ close . M.elems

socketHandler :: Callbacks c => SocketPlugin c -> ServerPartT IO Response
socketHandler plugin = dir "socket" $ msum $ map (\act -> act plugin) acts
    where acts = [connectHandler, sendHandler, closeHandler]

connectHandler :: Callbacks c => SocketPlugin c -> ServerPartT IO Response
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
    return $ toResponse "ok"

sendHandler :: Callbacks c => SocketPlugin c -> ServerPartT IO Response
sendHandler sp = dir "send" $ do
    nullDir
    callbackIndex <- look "callbackIndex"
    -- TODO: Maybe
    Just sock <- withSocket sp callbackIndex
    dataToSend <- look "data"
    -- TODO: resend until done
    _ <- liftIO $ send sock dataToSend
    return $ toResponse "ok"

closeHandler :: Callbacks c => SocketPlugin c -> ServerPartT IO Response
closeHandler sp = dir "close" $ do
    nullDir
    callbackIndex <- look "callbackIndex"
    sock <- withSocket sp callbackIndex
    case sock of
        Nothing -> return ()
        Just sock' -> liftIO $ do
            close sock'
            modifyMVar_ (spSock sp) $ return . M.delete callbackIndex
    return $ toResponse "ok"

withSocket :: MonadIO m => SocketPlugin c -> String -> m (Maybe Socket)
withSocket sp callbackIndex = liftIO $ withMVar (spSock sp) $ return . M.lookup callbackIndex
