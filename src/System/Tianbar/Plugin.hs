module System.Tianbar.Plugin where

import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson hiding (Array)
import Data.List.Split
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E

import Graphics.UI.Gtk.WebKit.WebView

import Network.URI

type UriHandler = String -> IO (Maybe String)

class Plugin p where
    initialize :: WebView -> IO p

    destroy :: p -> IO ()
    destroy _ = return ()

    handleRequest :: p -> UriHandler
    handleRequest _ _ = return Nothing

withScheme :: String -> (URI -> IO (Maybe String)) -> UriHandler
withScheme schemeMatch func uriStr
    | uriScheme uri == schemeMatch = func uri
    | otherwise = return Nothing
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

handleBlank :: IO (Maybe String) -> IO String
handleBlank = liftM $ fromMaybe (plainContent "")
