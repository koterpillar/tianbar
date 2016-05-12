module System.Tianbar.Plugin.DBus (DBusPlugin) where

-- DBus connectivity

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson (encode)
import qualified Data.Map as M

import Happstack.Server

import DBus.Client

import System.Tianbar.Callbacks
import System.Tianbar.Plugin
import System.Tianbar.Plugin.DBus.JSON ()
import System.Tianbar.Plugin.DBus.FromData ()

data Bus = Bus { busClient :: Client
               , busSignals :: MVar (M.Map String SignalHandler)
               }

busNew :: IO Client -> IO Bus
busNew conn = Bus <$> conn <*> newMVar M.empty

busDestroy :: Bus -> IO ()
-- TODO: remove signals?
busDestroy = disconnect . busClient

busAddListener :: (MonadIO m) => Bus -> String -> SignalHandler -> m ()
busAddListener bus index listener = liftIO $ modifyMVar_ (busSignals bus) $ \listeners ->
    return $ M.insert index listener listeners

busPopListener :: (MonadIO m) => Bus -> String -> m (Maybe SignalHandler)
busPopListener bus index = liftIO $ modifyMVar (busSignals bus) $ \listeners -> do
    let listener = M.lookup index listeners
    let listeners' = M.delete index listeners
    return (listeners', listener)

data DBusPlugin = DBusPlugin { dbusHost :: Callbacks
                             , dbusSession :: Bus
                             , dbusSystem :: Bus
                             }

busNameMap :: [(String, DBusPlugin -> Bus)]
busNameMap = [ ("session", dbusSession)
             , ("system", dbusSystem)
             ]

instance Plugin DBusPlugin where
    initialize c = do
        session <- busNew connectSession
        system <- busNew connectSystem
        return $ DBusPlugin c session system

    destroy plugin = mapM_ busDestroy [dbusSession plugin, dbusSystem plugin]

    handler plugin = dir "dbus" $ msum [ busHandler plugin busName (bus plugin)
                                       | (busName, bus) <- busNameMap
                                       ]

busHandler :: DBusPlugin -> String -> Bus -> ServerPartT IO Response
busHandler plugin busName bus = dir busName $ msum [ mzero
                                                   , listenHandler plugin bus
                                                   , stopHandler plugin bus
                                                   , callHandler plugin bus
                                                   ]

listenHandler :: DBusPlugin -> Bus -> ServerPartT IO Response
listenHandler plugin bus = dir "listen" $ withData $ \matcher -> do
    nullDir
    index <- look "index"
    listener <- liftIO $ addMatch (busClient bus) matcher $ \sig -> callback (dbusHost plugin) index [sig]
    busAddListener bus index listener
    return $ toResponse ("ok" :: String)

stopHandler :: DBusPlugin -> Bus -> ServerPartT IO Response
stopHandler _ bus = dir "stop" $ do
    nullDir
    index <- look "index"
    listener <- busPopListener bus index
    case listener of
        Just l -> do
            liftIO $ removeMatch (busClient bus) l
            return $ toResponse ("ok" :: String)
        Nothing -> mzero

callHandler :: DBusPlugin -> Bus -> ServerPartT IO Response
callHandler _ bus = dir "call" $ withData $ \mcall -> do
    nullDir
    res <- liftIO $ call (busClient bus) mcall
    return $ toResponse $ encode res
