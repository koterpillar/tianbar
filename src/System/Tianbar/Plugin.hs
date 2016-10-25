{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
module System.Tianbar.Plugin (
    ServerPart,
    Plugin (..),
    Response (..),
    FromData (..),
    URI (..),
    dir,
    look,
    lookBS,
    looks,
    looksBS,
    newCallback,
    nullDir,
    parseURI,
    path,
    runHandler,
    runPlugin,
    runWithLens,
    withData,
) where

import qualified Control.Lens as L

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Aeson
import qualified Data.Text as T

import System.Tianbar.Callbacks
import System.Tianbar.RequestResponse

type ServerPart s t = MaybeT (StateT s (ReaderT URI (StateT Callbacks IO))) t

runHandler :: MonadIO m => ServerPart p t -> p -> URI -> Callbacks -> m ((Maybe t, p), Callbacks)
runHandler h p uri cb = liftIO $ runStateT (runReaderT (runStateT (runMaybeT h) p) uri) cb

runWithLens :: L.Lens' p q -> ServerPart q Response -> ServerPart p Response
runWithLens l = mapMaybeT $ \act -> do
    p <- L.use l
    (res, p') <- lift $ runStateT act p
    L.assign l p'
    return res

runWithUri :: URI -> ServerPart p t -> ServerPart p t
runWithUri uri = mapMaybeT $ mapStateT $ local $ const uri

path :: (String -> ServerPart p t) -> ServerPart p t
path h = do
    uri <- ask
    let segments = uriPathSegments uri
    guard (not $ null segments)

    let uri' = uri { uriPathSegments = tail segments }
    runWithUri uri' $ h (T.unpack $ head segments)

dir :: String -> ServerPart p t -> ServerPart p t
dir name h = path $ \name' -> guard (name == name') >> h

nullDir :: ServerPart p ()
nullDir = do
    segments <- asks uriPathSegments
    guard $ null segments

withData :: FromData a => (a -> ServerPart p r) -> ServerPart p r
withData h = do
    d <- fromData
    h d

class Plugin p where
    initialize :: IO p

    destroy :: p -> IO ()
    destroy _ = return ()

    handler :: ServerPart p Response

runPlugin :: (Plugin p, MonadIO m) => p -> URI -> Callbacks -> m ((Maybe Response, p), Callbacks)
runPlugin = runHandler handler

newCallback :: ToJSON r => ServerPart p (Callback r, CallbackIndex)
newCallback = lift $ lift $ lift newCallbackT
