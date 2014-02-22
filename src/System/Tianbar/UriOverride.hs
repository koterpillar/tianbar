module System.Tianbar.UriOverride where

import Control.Monad

import Data.List.Split
import qualified Data.Map as M

import Network.URI

type UriOverride = String -> Maybe (IO String)

withScheme :: String -> (URI -> IO String) -> UriOverride
withScheme schemeMatch func uriStr
    | uriScheme uri == schemeMatch = Just $ func uri
    | otherwise = Nothing
    where (Just uri) = parseURI uriStr

mergeOverrides :: [UriOverride] -> UriOverride
mergeOverrides overrides = foldr mplus Nothing . flip map overrides . flip ($)

parseQuery :: URI -> M.Map String String
parseQuery = M.fromList
           . map ((\[a, b] -> (a, unEscapeString b)) . splitOn "=")
           . splitOn "&"
           . tail
           . uriQuery

returnContent :: String -> IO String
returnContent content = return $ "data:text/plain," ++ content
