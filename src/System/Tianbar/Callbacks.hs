{-# LANGUAGE OverloadedStrings #-}
module System.Tianbar.Callbacks (
    callback,
    callbacks,
    Callbacks,
) where

import Data.Aeson
import Data.Monoid

import GI.Gio.Objects.Cancellable

import GI.WebKit2.Objects.WebView

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as E

newtype Callbacks = Callbacks WebView

callbacks :: WebView -> Callbacks
callbacks = Callbacks

callback :: (ToJSON i, ToJSON p) => Callbacks -> i -> p -> IO ()
callback (Callbacks wk) index param =
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
