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
    looks,
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

import qualified Data.Text as T

import System.Tianbar.Callbacks
import System.Tianbar.RequestResponse

type ServerPart s t = MaybeT (CallbacksT (ReaderT URI (StateT s IO))) t

runHandler :: (MonadIO m, CallbackHost h) => ServerPart p t -> p -> URI -> h -> m (Maybe t, p)
runHandler h p uri host = liftIO $ runStateT (runReaderT (runCallbacks host (runMaybeT h)) uri) p

runWithLens :: L.Lens' p q -> ServerPart q Response -> ServerPart p Response
runWithLens l h' = MaybeT $ runCallbacksWith (runReaderWithLens l) (runMaybeT h')
    where runReaderWithLens :: L.Lens' p q -> forall a b. ReaderT b (StateT q IO) a -> ReaderT b (StateT p IO) a
          runReaderWithLens l' r = ReaderT $ \u -> do
              s <- L.use l'
              (res, s') <- lift $ runStateT (runReaderT r u) s
              L.assign l' s'
              return res

runWithUri :: URI -> ServerPart p t -> ServerPart p t
runWithUri uri' h = MaybeT $ runCallbacksWith (replaceInput uri') (runMaybeT h)
    where replaceInput :: b -> forall a. ReaderT b m a -> ReaderT b m a
          replaceInput inp' r = ReaderT $ \_ -> runReaderT r inp'

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

runPlugin :: (Plugin p, MonadIO m, CallbackHost h) => p -> URI -> h -> m (Maybe Response, p)
runPlugin = runHandler handler
