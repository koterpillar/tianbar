module System.Tianbar.Plugin where

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

import Graphics.UI.Gtk.WebKit.WebView

import Network.URI

type UriHandler = String -> Maybe (IO String)

class Plugin p where
    initialize :: IO p
    initialize = return simpleInitialize

    simpleInitialize :: p

    destroy :: p -> IO ()
    destroy _ = return ()

    handleRequest :: p -> WebView -> UriHandler
    handleRequest p _ = simpleHandleRequest p

    simpleHandleRequest :: p -> UriHandler
    simpleHandleRequest _ _ = Nothing

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

getQueryParam :: String -> URIParams -> String
getQueryParam key = fromJust . lookupQueryParam key

getQueryParams :: String -> URIParams -> [String]
getQueryParams key = fromMaybe [] . M.lookup key

plainContent :: String -> String
plainContent content = "data:text/plain," ++ content

returnContent :: String -> IO String
returnContent = return . plainContent
