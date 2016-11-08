{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Tianbar.Callbacks (
    Callback,
    Callbacks,
    CallbackIndex,
    CallbackHost,
    callbacks,
    callbackResponse,
    newCallbackT,
) where

import Control.Lens hiding ((.=))

import Control.Monad
import Control.Monad.State

import Data.Aeson
import Data.Monoid

import GI.Gio.Objects.Cancellable

import GI.GLib.Constants
import GI.GLib.Functions

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

type Callback r = r -> IO ()

class CallbackHost h where
    callback :: (ToJSON i, ToJSON r) => h -> i -> Callback r

instance CallbackHost WebView where
    callback wk idx param = do
        _ <- idleAdd PRIORITY_DEFAULT_IDLE $ do
            webViewRunJavascript wk
                (TL.toStrict $ callbackScript idx param)
                noCancellable
                Nothing
            return False
        return ()

callbackScript :: (ToJSON i, ToJSON r) => i -> r -> TL.Text
callbackScript idx param =
    "window.tianbarEvents && " <> eventStr <> " && "
        <> eventStr <> ".fire.apply(" <> eventStr <> ", " <> paramStr <> ")"
    where eventStr = "window.tianbarEvents[" <> indexStr <> "]"
          indexStr = showJSON idx
          paramStr = showJSON param

showJSON :: ToJSON a => a -> TL.Text
showJSON = E.decodeUtf8 . encode

type CallbackHostFunc = Int -> Callback Value

data Callbacks = Callbacks { _cbHost :: CallbackHostFunc
                           , _cbNextIndex :: Int
                           }

cbHost :: Getter Callbacks CallbackHostFunc
cbHost inj (Callbacks h i) = flip Callbacks i <$> inj h

cbNextIndex :: Lens' Callbacks Int
cbNextIndex inj (Callbacks h i) = Callbacks h <$> inj i

callbacks :: CallbackHost h => h -> Callbacks
callbacks h = Callbacks (callback h) 0

callbackResponse :: Monad m => CallbackIndex -> m Response
callbackResponse (CallbackIndex idx) = jsonResponse $ object [ "callbackIndex" .= show idx ]

newCallbackT :: (MonadState Callbacks m, ToJSON r) => m (Callback r, CallbackIndex)
newCallbackT = do
    host <- use cbHost
    newIndex <- use cbNextIndex
    cbNextIndex += 1
    return (host newIndex . toJSON, CallbackIndex newIndex)
