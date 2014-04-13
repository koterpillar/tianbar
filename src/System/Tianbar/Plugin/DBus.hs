module System.Tianbar.Plugin.DBus where

-- DBus connectivity

import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson (encode)

import Happstack.Server

import DBus.Client

import System.Tianbar.Callbacks
import System.Tianbar.Plugin
import System.Tianbar.Plugin.DBus.JSON ()
import System.Tianbar.Plugin.DBus.FromData ()

data DBusPlugin = DBusPlugin { dbusHost :: Callbacks
                             , dbusSession :: Client
                             , dbusSystem :: Client
                             }

busNameMap :: [(String, DBusPlugin -> Client)]
busNameMap = [ ("session", dbusSession)
             , ("system", dbusSystem)
             ]

instance Plugin DBusPlugin where
    initialize c = do
        session <- connectSession
        system <- connectSystem
        return $ DBusPlugin c session system

    destroy plugin = mapM_ disconnect [dbusSession plugin, dbusSystem plugin]

    handler plugin = dir "dbus" $ msum [ busHandler plugin busName (bus plugin)
                                       | (busName, bus) <- busNameMap
                                       ]

busHandler :: DBusPlugin -> String -> Client -> ServerPartT IO Response
busHandler plugin busName bus = dir busName $ msum [ mzero
                                                   , listenHandler plugin bus
                                                   , callHandler plugin bus
                                                   ]

listenHandler :: DBusPlugin -> Client -> ServerPartT IO Response
listenHandler plugin client = dir "listen" $ withData $ \matcher -> do
    nullDir
    index <- look "index"
    _ <- liftIO $ addMatch client matcher $ \sig -> callback (dbusHost plugin) index [sig]
    return $ toResponse ("ok" :: String)

callHandler :: DBusPlugin -> Client -> ServerPartT IO Response
callHandler _ client = dir "call" $ withData $ \mcall -> do
    nullDir
    res <- liftIO $ call client mcall
    return $ toResponse $ encode res
