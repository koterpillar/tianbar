{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module System.Tianbar.Plugin (
    ServerPart,
    Plugin (..),
    Response (..),
    FromData (..),
    URI (..),
    bytestringResponse,
    dir,
    callbackResponse,
    jsonResponse,
    look,
    looks,
    nullDir,
    okResponse,
    parseURI,
    path,
    runHandler,
    runPlugin,
    serveFile,
    stringResponse,
    textResponse,
    withData,
) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as U
import qualified Data.ByteString.Lazy.Char8 as LBS
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

type ServerPart s t = MaybeT (ReaderT URI (StateT s IO)) t

runHandler :: MonadIO m => ServerPart p t -> p -> URI -> m (Maybe t, p)
runHandler h p uri = liftIO $ runStateT (runReaderT (runMaybeT h) uri) p

withUri :: URI -> ServerPart p t -> ServerPart p t
withUri newUri h = do
    p <- get
    (res, p') <- liftIO $ runHandler h p newUri
    put p'
    MaybeT $ return res

path :: (String -> ServerPart p t) -> ServerPart p t
path h = do
    uri <- ask
    let segments = uriPathSegments uri
    guard (not $ null segments)

    let uri' = uri { uriPathSegments = tail segments }
    withUri uri' $ h (T.unpack $ head segments)

dir :: String -> ServerPart p t -> ServerPart p t
dir name h = path $ \name' -> guard (name == name') >> h

nullDir :: ServerPart p ()
nullDir = do
    segments <- asks uriPathSegments
    guard $ null segments

look :: String -> ServerPart p String
look param = looks param >>= \values -> case values of
               [value] -> return value
               _ -> mzero

looks :: String -> ServerPart p [String]
looks param = asks uriQuery >>= \params -> do
    let paramTxt = U.fromString param
    let isParam (p, value) | p == paramTxt = fmap U.toString value
                           | otherwise = Nothing
    MaybeT $ return $ Just $ mapMaybe isParam params

class FromData a where
    fromData :: forall p. ServerPart p a

instance FromData a => FromData (Maybe a) where
    fromData = fmap Just fromData `mplus` return Nothing

withData :: FromData a => (a -> ServerPart p r) -> ServerPart p r
withData h = do
    d <- fromData
    h d

serveFile :: FilePath -> ServerPart p Response
serveFile filePath = do
    contents <- liftIO $ B.readFile filePath
    let fileType = defaultMimeLookup $ T.pack $ takeFileName filePath
    -- FIXME: Guess the MIME type
    return $ Response contents (Just $ U.toString fileType)

stringResponse :: String -> ServerPart p Response
stringResponse = bytestringResponse . U.fromString

textResponse :: T.Text -> ServerPart p Response
textResponse = bytestringResponse . TE.encodeUtf8

bytestringResponse :: B.ByteString -> ServerPart p Response
bytestringResponse str = return $ Response str (Just "text/plain")

okResponse :: ServerPart p Response
okResponse = bytestringResponse "ok"

jsonResponse :: ToJSON v => v -> ServerPart p Response
jsonResponse = bytestringResponse . LBS.toStrict . encode

callbackResponse :: String -> ServerPart p Response
callbackResponse idx = jsonResponse $ object [ "callbackIndex" .= idx ]

class Plugin p where
    initialize :: Callbacks -> IO p

    destroy :: p -> IO ()
    destroy _ = return ()

    handler :: ServerPart p Response

runPlugin :: (Plugin p, MonadIO m) => p -> URI -> m (Maybe Response, p)
runPlugin = runHandler handler
