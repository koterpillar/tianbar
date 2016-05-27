{-# LANGUAGE OverloadedStrings #-}
module System.Tianbar.Plugin (
    Handler,
    Plugin (..),
    Response (..),
    FromData (..),
    URI (..),
    bytestringResponse,
    dir,
    look,
    looks,
    nullDir,
    okResponse,
    parseURI,
    path,
    runPlugin,
    serveFile,
    stringResponse,
    textResponse,
    withData,
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Network.Mime (defaultMimeLookup)
import Network.HTTP.Types (Query, decodePath)

import System.FilePath

import System.Tianbar.Callbacks

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

type Handler t = MaybeT (ReaderT URI IO) t

runHandler :: MonadIO m => Handler t -> URI -> m (Maybe t)
runHandler h uri = liftIO $ runReaderT (runMaybeT h) uri

withUri :: URI -> Handler t -> Handler t
withUri newUri h = do
    res <- liftIO $ runHandler h newUri
    MaybeT $ return res

path :: (String -> Handler t) -> Handler t
path h = do
    uri <- lift ask
    let segments = uriPathSegments uri
    guard (not $ null segments)

    let uri' = uri { uriPathSegments = tail segments }
    withUri uri' $ h (T.unpack $ head segments)

dir :: String -> Handler t -> Handler t
dir name h = path $ \name' -> guard (name == name') >> h

nullDir :: Handler ()
nullDir = do
    segments <- lift $ asks uriPathSegments
    guard $ null segments

look :: String -> Handler String
look param = looks param >>= \values -> case values of
               [value] -> return value
               _ -> mzero

looks :: String -> Handler [String]
looks param = lift (asks uriQuery) >>= \params -> do
    let paramTxt = U.fromString param
    let isParam (p, value) | p == paramTxt = liftM U.toString value
                           | otherwise = Nothing
    MaybeT $ return $ Just $ catMaybes $ map isParam params

class FromData a where
    fromData :: Handler a

instance FromData a => FromData (Maybe a) where
    fromData = liftM Just fromData `mplus` return Nothing

withData :: FromData a => (a -> Handler r) -> Handler r
withData h = do
    d <- fromData
    h d

serveFile :: FilePath -> Handler Response
serveFile filePath = do
    contents <- liftIO $ B.readFile filePath
    let fileType = defaultMimeLookup $ T.pack $ takeFileName filePath
    -- FIXME: Guess the MIME type
    return $ Response contents (Just $ U.toString fileType)

stringResponse :: String -> Handler Response
stringResponse = bytestringResponse . U.fromString

textResponse :: T.Text -> Handler Response
textResponse = bytestringResponse . TE.encodeUtf8

bytestringResponse :: B.ByteString -> Handler Response
bytestringResponse str = return $ Response str (Just "text/plain")

okResponse :: Handler Response
okResponse = bytestringResponse "ok"

class Plugin p where
    initialize :: Callbacks -> IO p

    destroy :: p -> IO ()
    destroy _ = return ()

    handler :: p -> Handler Response

runPlugin :: (Plugin p, MonadIO m) => p -> URI -> m (Maybe Response)
runPlugin = runHandler . handler
