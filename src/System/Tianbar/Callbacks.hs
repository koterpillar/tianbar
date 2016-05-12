{-# LANGUAGE OverloadedStrings #-}
module System.Tianbar.Callbacks (
    callback,
    callbacks,
    Callbacks,
) where

import Data.Aeson

import GI.Signals

import GI.WebKit2.Objects.WebView

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as E

newtype Callbacks = Callbacks WebView

callbacks :: WebView -> Callbacks
callbacks = Callbacks

callback :: (ToJSON i, ToJSON p) => Callbacks -> i -> p -> IO ()
callback (Callbacks wk) index param =
    undefined
    -- webViewRunJavascript wk (T.pack $ callbackScript index param) Nothing Nothing

callbackScript :: (ToJSON i, ToJSON p) => i -> p -> String
callbackScript index param =
    "window.tianbarEvents && " ++ eventStr ++ " && "
        ++ eventStr ++ ".fire.apply(" ++ eventStr ++ ", " ++ paramStr ++ ")"
    where eventStr = "window.tianbarEvents[" ++ indexStr ++ "]"
          indexStr = showJSON index
          paramStr = showJSON param

showJSON :: ToJSON a => a -> String
showJSON = TL.unpack . E.decodeUtf8 . encode . toJSON
