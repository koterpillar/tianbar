module System.Tianbar.Callbacks where

import Data.Aeson

import Graphics.UI.Gtk
import Graphics.UI.Gtk.WebKit.WebView

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E

class Callbacks c where
    callback :: (ToJSON i, ToJSON p) => c -> i -> p -> IO ()

instance Callbacks WebView where
    callback wk index param =
        postGUIAsync $ webViewExecuteScript wk $ callbackScript index param

callbackScript :: (ToJSON i, ToJSON p) => i -> p -> String
callbackScript index param =
    "window.tianbarEvents && " ++ eventStr ++ " && "
        ++ eventStr ++ ".fire.apply(" ++ eventStr ++ ", " ++ paramStr ++ ")"
    where eventStr = "window.tianbarEvents[" ++ indexStr ++ "]"
          indexStr = showJSON index
          paramStr = showJSON param

showJSON :: ToJSON a => a -> String
showJSON = T.unpack . E.decodeUtf8 . encode . toJSON
