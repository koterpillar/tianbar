{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module System.Tianbar.RequestResponse (
    URI (..),
    FromData (..),
    Response (..),
    bytestringResponse,
    jsonResponse,
    look,
    looks,
    okResponse,
    parseURI,
    serveFile,
    stringResponse,
    textResponse,
) where

import Control.Monad.IO.Class
import Control.Monad.Reader

import Data.Aeson

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.UTF8 as U

import Data.Maybe

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Network.HTTP.Types (Query, decodePath)
import Network.Mime (defaultMimeLookup)

import System.FilePath


data URI = URI { uriPathSegments :: [T.Text]
               , uriQuery :: Query
               }
               deriving (Show)

parseURI :: T.Text -> URI
parseURI str = URI segments query
    where (segments, query) = decodePath uriPath
          uriPath = extractPath $ TE.encodeUtf8 str
          prefix = U.fromString "tianbar://"
          extractPath uri | prefix `B.isPrefixOf` uri = B.drop (B.length prefix) uri
                          | otherwise = uri

data Response = Response { content :: B.ByteString
                         , mimeType :: Maybe String
                         }

stringResponse :: Monad m => String -> m Response
stringResponse = bytestringResponse . U.fromString

textResponse :: Monad m => T.Text -> m Response
textResponse = bytestringResponse . TE.encodeUtf8

bytestringResponse :: Monad m => B.ByteString -> m Response
bytestringResponse str = return $ Response str (Just "text/plain")

okResponse :: Monad m => m Response
okResponse = bytestringResponse "ok"

jsonResponse :: (Monad m, ToJSON v) => v -> m Response
jsonResponse = bytestringResponse . LBS.toStrict . encode

serveFile :: MonadIO m => FilePath -> m Response
serveFile filePath = do
    contents <- liftIO $ B.readFile filePath
    let fileType = defaultMimeLookup $ T.pack $ takeFileName filePath
    -- FIXME: Guess the MIME type
    return $ Response contents (Just $ U.toString fileType)

look :: (MonadPlus m, MonadReader URI m) => String -> m String
look param = looks param >>= \values -> case values of
               [value] -> return value
               _ -> mzero

looks :: (MonadPlus m, MonadReader URI m) => String -> m [String]
looks param = asks uriQuery >>= \params -> do
    let paramTxt = U.fromString param
    let isParam (p, value) | p == paramTxt = fmap U.toString value
                           | otherwise = Nothing
    return $ mapMaybe isParam params

class FromData a where
    fromData :: (MonadPlus m, MonadReader URI m) => m a

instance FromData a => FromData (Maybe a) where
    fromData = fmap Just fromData `mplus` return Nothing
