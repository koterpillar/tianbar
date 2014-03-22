module System.Tianbar.UriOverride where

import Control.Monad

import Data.List.Split
import qualified Data.Map as M
import Data.Maybe

import Network.URI

type UriOverride = String -> Maybe (IO String)

withScheme :: String -> (URI -> IO String) -> UriOverride
withScheme schemeMatch func uriStr
    | uriScheme uri == schemeMatch = Just $ func uri
    | otherwise = Nothing
    where (Just uri) = parseURI uriStr

mergeOverrides :: [UriOverride] -> UriOverride
mergeOverrides overrides = foldr mplus Nothing . flip map overrides . flip ($)

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
