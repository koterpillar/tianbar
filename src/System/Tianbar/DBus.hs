{-# Language OverloadedStrings #-}
{-# Language FlexibleInstances #-}
{-# Language RankNTypes #-}
module System.Tianbar.DBus (dbus) where

-- DBus connectivity

import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson (encode)
import Data.List.Split
import Data.Maybe

import Happstack.Server

import DBus
import DBus.Client

import System.Tianbar.DBus.JSON ()
import System.Tianbar.Callbacks

data DBusPlugin c = DBusPlugin { dbusHost :: c
                               , dbusSession :: Client
                               , dbusSystem :: Client
                               }

busNameMap :: forall c. [(String, DBusPlugin c -> Client)]
busNameMap = [ ("session", dbusSession)
             , ("system", dbusSystem)
             ]

-- TODO: Is IO needed twice?
dbus :: Callbacks c => c -> IO (ServerPartT IO Response)
dbus c = do
    session <- connectSession
    system <- connectSystem
    return $ dbusHandler $ DBusPlugin c session system

-- TODO: stopping?
destroy :: DBusPlugin c -> IO ()
destroy plugin = mapM_ disconnect [dbusSession plugin, dbusSystem plugin]

dbusHandler :: Callbacks c => DBusPlugin c -> ServerPartT IO Response
dbusHandler plugin = dir "dbus" $ msum [ busHandler plugin busName (bus plugin)
                                       | (busName, bus) <- busNameMap
                                       ]

busHandler :: Callbacks c => DBusPlugin c -> String -> Client -> ServerPartT IO Response
busHandler plugin busName bus = dir busName $ msum [ mzero
                                                   , listenHandler plugin bus
                                                   , callHandler plugin bus
                                                   ]

listenHandler :: Callbacks c => DBusPlugin c -> Client -> ServerPartT IO Response
listenHandler plugin client = dir "listen" $ withData $ \matcher -> do
    nullDir
    index <- look "index"
    _ <- liftIO $ addMatch client matcher $ \sig -> callback (dbusHost plugin) index [sig]
    return $ toResponse ("ok" :: String)

callHandler :: DBusPlugin c -> Client -> ServerPartT IO Response
callHandler _ client = dir "call" $ withData $ \mcall -> do
    nullDir
    res <- liftIO $ call client mcall
    return $ toResponse $ encode res

instance FromData MatchRule where
    fromData = do
        objPath <- fromData
        iface <- fromData
        memberName <- fromData
        return $ matchAny { matchSender = Nothing
                          , matchDestination = Nothing
                          , matchPath = objPath
                          , matchInterface = iface
                          , matchMember = memberName
                          }

instance FromData MethodCall where
    fromData = do
        callPath <- fromData
        iface <- fromData
        member <- fromData
        callBody <- liftM (map variantFromString) $ looks "body[]"
        dest <- fromData
        let setBodyDest mcall = mcall { methodCallBody = callBody
                                      , methodCallDestination = dest
                                      }
        return $ setBodyDest $ methodCall callPath iface member

instance FromData ObjectPath where
    fromData = liftM (fromJust . parseObjectPath) $ look "path"

instance FromData InterfaceName where
    fromData = liftM (fromJust . parseInterfaceName) $ look "iface"

instance FromData MemberName where
    fromData = liftM (fromJust . parseMemberName) $ look "member"

instance FromData BusName where
    fromData = liftM (fromJust . parseBusName) $ look "destination"

variantFromString :: String -> Variant
variantFromString param = case splitOn ":" param of
    ["string", str] -> toVariant str
    _ -> error "Invalid variant string"
