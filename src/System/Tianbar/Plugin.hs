module System.Tianbar.Plugin where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Data.Aeson hiding (Array)
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.WebKit.WebView

import Network.URI

type UriHandler = String -> Maybe (IO String)

class Plugin p where
    initialize :: WebView -> IO p

    destroy :: p -> IO ()
    destroy _ = return ()

    handleRequest :: p -> UriHandler
    handleRequest _ _ = Nothing

withScheme :: String -> (URI -> IO String) -> UriHandler
withScheme schemeMatch func uriStr
    | uriScheme uri == schemeMatch = Just $ func uri
    | otherwise = Nothing
    where (Just uri) = parseURI uriStr

multiMap :: [(String, String)] -> M.Map String [String]
multiMap = foldr addElement M.empty
    where addElement (k, v) = M.insertWith (++) k [v]

type URIParams = M.Map String [String]

parseQuery :: URI -> URIParams
parseQuery = multiMap
           . map ((\[a, b] -> (a, b)) . map unEscapeString . splitOn "=")
           . splitOn "&"
           . tail
           . uriQuery

lookupQueryParam :: String -> URIParams -> Maybe String
lookupQueryParam key queryMap = M.lookup key queryMap >>= \value -> case value of
    [v] -> Just v
    _ -> Nothing

getQueryParams :: String -> URIParams -> [String]
getQueryParams key = fromMaybe [] . M.lookup key

plainContent :: String -> String
plainContent content = "data:text/plain," ++ content

returnContent :: MonadIO m => String -> m String
returnContent = return . plainContent

returnJSON :: (MonadIO m, ToJSON a) => a -> m String
returnJSON = returnContent . T.unpack . E.decodeUtf8 . encode . toJSON

callback :: (ToJSON i, ToJSON p) => WebView -> i -> p -> IO ()
callback wk index param =
    Gtk.postGUIAsync $ webViewExecuteScript wk $ callbackScript index param

callbackScript :: (ToJSON i, ToJSON p) => i -> p -> String
callbackScript index param =
    "window.tianbarEvents && " ++ eventStr ++ " && "
        ++ eventStr ++ ".fire.apply(" ++ eventStr ++ ", " ++ paramStr ++ ")"
    where eventStr = "window.tianbarEvents[" ++ indexStr ++ "]"
          indexStr = showJSON index
          paramStr = showJSON param

showJSON :: ToJSON a => a -> String
showJSON = T.unpack . E.decodeUtf8 . encode . toJSON

liftMT :: Monad m => Maybe a -> MaybeT m a
liftMT = MaybeT . return

handleBlank :: IO (Maybe String) -> IO String
handleBlank = liftM $ fromMaybe (plainContent "")
