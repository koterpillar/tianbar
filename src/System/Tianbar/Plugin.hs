module System.Tianbar.Plugin (
    Handler,
    Plugin (..),
    Response (..),
    FromData (..),
    dir,
    look,
    looks,
    nullDir,
    path,
    runPlugin,
    serveFile,
    stringResponse,
    withData,
) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

import qualified Data.List as L
import Data.List.Split
import Data.Maybe

import Network.URI (URI, uriPath, uriQuery)

import System.Tianbar.Callbacks

data Response = Response { content :: String
                         , mimeType :: Maybe String
                         }

type Handler t = MaybeT (ReaderT URI IO) t

runHandler :: MonadIO m => Handler t -> URI -> m (Maybe t)
runHandler h uri = liftIO $ runReaderT (runMaybeT h) uri

pathSegments :: URI -> [String]
pathSegments = filter (not . null) . splitOn "/" . uriPath

segmentsPath :: [String] -> String
segmentsPath = ('/':) . L.intercalate "/"

withUri :: URI -> Handler t -> Handler t
withUri newUri h = do
    res <- liftIO $ runHandler h newUri
    MaybeT $ return res

dir :: String -> Handler t -> Handler t
dir name h = do
    uri <- lift ask
    let segments = pathSegments uri
    guard (not $ null segments)
    guard (head segments == name)
    let uri' = uri { uriPath = segmentsPath (tail segments) }
    withUri uri' h

path :: (String -> Handler t) -> Handler t
path h = do
    uri <- lift ask
    let segments = pathSegments uri
    guard (not $ null segments)

    let uri' = uri { uriPath = segmentsPath (tail segments) }
    withUri uri' $ h (head segments)

nullDir :: Handler ()
nullDir = do
    segments <- lift $ asks pathSegments
    guard $ null segments

look :: String -> Handler String
look param = looks param >>= \values -> case values of
               [value] -> return value
               _ -> mzero

looks :: String -> Handler [String]
looks param = lift (asks uriQuery) >>= \uquery -> do
    -- TODO: More efficient implementation
    let params = map (splitOn "=") $ splitOn "&" uquery
    let isParam [p, value] | p == param = Just value
                           | otherwise = Nothing
        isParam _ = Nothing
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
    contents <- liftIO $ readFile filePath
    -- FIXME: Guess the MIME type
    return $ Response contents Nothing

stringResponse :: String -> Handler Response
stringResponse str = return $ Response str (Just "text/plain")

class Plugin p where
    initialize :: Callbacks -> IO p

    destroy :: p -> IO ()
    destroy _ = return ()

    handler :: p -> Handler Response

runPlugin :: (Plugin p, MonadIO m) => p -> URI -> m (Maybe Response)
runPlugin = runHandler . handler
