{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
module System.Tianbar.Callbacks (
    Callback,
    CallbackIndex,
    CallbackHost,
    CallbacksT,
    callbackResponse,
    newCallback,
    runCallbacks,
    runCallbacksWith,
) where

import Control.Applicative

import Control.Arrow (second)

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Data.Aeson
import Data.Monoid

import GI.Gio.Objects.Cancellable

import GI.WebKit2.Objects.WebView

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as E

import Text.Read (readMaybe)

import System.Tianbar.RequestResponse

newtype CallbackIndex = CallbackIndex Int
    deriving (Eq, Ord)

instance FromData CallbackIndex where
    fromData = do
        idxStr <- look "callbackIndex"
        case readMaybe idxStr of
          Just idx -> return $ CallbackIndex idx
          Nothing -> mzero

type Callback p = p -> IO ()

class CallbackHost h where
    callback :: (ToJSON i, ToJSON p) => h -> i -> Callback p

{- FIXME: Move to WebKit -}
instance CallbackHost WebView where
    callback wk index param =
        webViewRunJavascript wk
            (TL.toStrict $ callbackScript index param)
            noCancellable
            Nothing

callbackScript :: (ToJSON i, ToJSON p) => i -> p -> TL.Text
callbackScript index param =
    "window.tianbarEvents && " <> eventStr <> " && "
        <> eventStr <> ".fire.apply(" <> eventStr <> ", " <> paramStr <> ")"
    where eventStr = "window.tianbarEvents[" <> indexStr <> "]"
          indexStr = showJSON index
          paramStr = showJSON param

showJSON :: ToJSON a => a -> TL.Text
showJSON = E.decodeUtf8 . encode
{- FIXME: end Move to WebKit -}

data CallbacksT m a = CallbacksT { runCallbacksT :: forall h. CallbackHost h => h -> Int -> m (a, Int) }

instance (Functor m) => Functor (CallbacksT m) where
    fmap f m = CallbacksT $ \h s ->
        fmap (\ ~(a, s') -> (f a, s')) $ runCallbacksT m h s
    {-# INLINE fmap #-}

instance (Functor m, Monad m) => Applicative (CallbacksT m) where
    pure a = CallbacksT $ \_ s -> return (a, s)
    {-# INLINE pure #-}
    CallbacksT mf <*> CallbacksT mx = CallbacksT $ \h s -> do
        ~(f, s') <- mf h s
        ~(x, s'') <- mx h s'
        return (f x, s'')
    {-# INLINE (<*>) #-}

instance (Functor m, MonadPlus m) => Alternative (CallbacksT m) where
    empty = CallbacksT $ \_ _ -> mzero
    {-# INLINE empty #-}
    CallbacksT m <|> CallbacksT n = CallbacksT $ \h s -> m h s `mplus` n h s
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (CallbacksT m) where
    a >>= b = CallbacksT $ \h i -> do
        (ra, i') <- runCallbacksT a h i
        runCallbacksT (b ra) h i'
    return a = CallbacksT $ \_ i -> return (a, i)
    {-# INLINE return #-}

instance MonadTrans CallbacksT where
    lift act = CallbacksT $ \_ n -> liftM (,n) act

instance MonadState s m => MonadState s (CallbacksT m) where
    get = lift get
    put v = lift $ put v

instance MonadReader s m => MonadReader s (CallbacksT m) where
    ask = lift ask
    local f act = CallbacksT $ \h i -> local f $ runCallbacksT act h i

instance MonadIO m => MonadIO (CallbacksT m) where
    liftIO = lift . liftIO

runCallbacks :: (Monad m, CallbackHost h) => h -> CallbacksT m a -> m (a, CallbackIndex)
runCallbacks = runCallbacksFrom (CallbackIndex 0)

runCallbacksFrom :: (Monad m, CallbackHost h) => CallbackIndex -> h -> CallbacksT m a -> m (a, CallbackIndex)
runCallbacksFrom (CallbackIndex start) h cb = liftM (second CallbackIndex) $ runCallbacksT cb h start

runCallbacksWith :: (Monad m, Monad m') => (forall a. m a -> m' a) -> CallbacksT m b -> CallbacksT m' b
runCallbacksWith lft cb = CallbacksT $ \h n -> lft $ runCallbacksT cb h n

callbackResponse :: Monad m => CallbackIndex -> m Response
callbackResponse (CallbackIndex idx) = jsonResponse $ object [ "callbackIndex" .= show idx ]

newCallback :: (Monad m, ToJSON p) => CallbacksT m (Callback p, CallbackIndex)
newCallback = CallbacksT $ \h i ->
    return ((callback h i, CallbackIndex i), i + 1)
