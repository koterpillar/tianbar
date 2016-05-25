module System.Tianbar.Plugin.DBus (DBusPlugin) where

-- DBus connectivity

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M

import DBus (parseAddress)
import DBus.Client ( Client
                   , SignalHandler
                   , addMatch
                   , call
                   , connect
                   , connectSystem
                   , connectSession
                   , disconnect
                   , removeMatch
                   )

import System.Tianbar.Callbacks
import System.Tianbar.Plugin
import System.Tianbar.Plugin.DBus.JSON ()
import System.Tianbar.Plugin.DBus.FromData ()

insertMVar :: Ord k => MonadIO m => k -> v -> MVar (M.Map k v) -> m ()
insertMVar k v m = liftIO $ modifyMVar_ m $ return . M.insert k v

data Bus = Bus { busClient :: Client
               , busSignals :: MVar (M.Map String SignalHandler)
               }

busNew :: IO Client -> IO Bus
busNew conn = Bus <$> conn <*> newMVar M.empty

busDestroy :: Bus -> IO ()
-- TODO: remove signals?
busDestroy = disconnect . busClient

busAddListener :: (MonadIO m) => Bus -> String -> SignalHandler -> m ()
busAddListener bus index listener = insertMVar index listener (busSignals bus)

busPopListener :: (MonadIO m) => Bus -> String -> m (Maybe SignalHandler)
busPopListener bus index = liftIO $ modifyMVar (busSignals bus) $ \listeners -> do
    let listener = M.lookup index listeners
    let listeners' = M.delete index listeners
    return (listeners', listener)

data DBusPlugin = DBusPlugin { dbusHost :: Callbacks
                             , dbusMap :: MVar (M.Map String Bus)
                             }

instance Plugin DBusPlugin where
    initialize c = do
        busMap <- newMVar M.empty
        session <- busNew connectSession
        insertMVar "session" session busMap
        system <- busNew connectSystem
        insertMVar "system" system busMap
        return $ DBusPlugin c busMap

    destroy plugin = withMVar (dbusMap plugin) $ \m -> forM_ m $ busDestroy

    handler plugin = dir "dbus" $ msum [ busesHandler plugin
                                       , connectBusHandler plugin
                                       ]

busesHandler :: DBusPlugin -> Handler Response
busesHandler plugin = path $ \busName -> do
    bus <- liftIO $ withMVar (dbusMap plugin) $ return . M.lookup busName
    case bus of
        Just bus' -> busHandler plugin bus'
        Nothing -> mzero

connectBusHandler :: DBusPlugin -> Handler Response
connectBusHandler plugin = dir "connect" $ do
    nullDir
    name <- look "name"
    addressStr <- look "address"
    address <- MaybeT $ return $ parseAddress addressStr
    bus <- liftIO $ busNew $ connect address
    insertMVar name bus (dbusMap plugin)
    stringResponse "ok"

busHandler :: DBusPlugin -> Bus -> Handler Response
busHandler plugin bus = msum [ listenHandler plugin bus
                             , stopHandler plugin bus
                             , callHandler plugin bus
                             ]

listenHandler :: DBusPlugin -> Bus -> Handler Response
listenHandler plugin bus = dir "listen" $ withData $ \matcher -> do
    nullDir
    index <- look "index"
    listener <- liftIO $ addMatch (busClient bus) matcher $ \sig -> callback (dbusHost plugin) index [sig]
    busAddListener bus index listener
    stringResponse "ok"

stopHandler :: DBusPlugin -> Bus -> Handler Response
stopHandler _ bus = dir "stop" $ do
    nullDir
    index <- look "index"
    listener <- busPopListener bus index
    case listener of
        Just l -> do
            liftIO $ removeMatch (busClient bus) l
            stringResponse "ok"
        Nothing -> mzero

callHandler :: DBusPlugin -> Bus -> Handler Response
callHandler _ bus = dir "call" $ withData $ \mcall -> do
    nullDir
    res <- liftIO $ call (busClient bus) mcall
    stringResponse $ BS.unpack $ encode res
