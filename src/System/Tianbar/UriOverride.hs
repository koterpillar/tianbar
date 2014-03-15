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

parseQuery :: URI -> M.Map String [String]
parseQuery = foldr (uncurry (M.insertWith (++))) M.empty
           . map ((\[a, b] -> (a, [b])) . map unEscapeString . splitOn "=")
           . splitOn "&"
           . tail
           . uriQuery

lookupQueryParam :: String -> M.Map String [String] -> Maybe String
lookupQueryParam key queryMap = M.lookup key queryMap >>= \value -> case value of
    [v] -> Just v
    _ -> Nothing

getQueryParam :: String -> M.Map String [String] -> String
getQueryParam key = fromJust . lookupQueryParam key

getQueryParams :: String -> M.Map String [String] -> [String]
getQueryParams key = fromMaybe [] . M.lookup key

returnContent :: String -> IO String
returnContent content = return $ "data:text/plain," ++ content
