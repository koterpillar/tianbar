{-# LANGUAGE RankNTypes #-}
module System.Tianbar.Plugin.DBus (DBusPlugin) where

-- DBus connectivity

import Control.Lens hiding (index)

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

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


type SignalMap = M.Map String SignalHandler

data Bus = Bus { _busClient :: Client
               , _busSignals :: SignalMap
               }

busClient :: Getter Bus Client
busClient inj (Bus c s) = flip Bus s <$> inj c

busSignals :: Lens' Bus SignalMap
busSignals inj (Bus c s) = Bus c <$> inj s

busNew :: IO Client -> IO Bus
busNew conn = Bus <$> conn <*> pure M.empty

busDestroy :: Bus -> IO ()
busDestroy bus = do
    let clnt = bus ^. busClient
    forM_ (bus ^. busSignals) $ removeMatch clnt
    disconnect clnt

type BusMap = M.Map String Bus

data DBusPlugin = DBusPlugin { _dbusHost :: Callbacks
                             , _dbusMap :: BusMap
                             }

dbusMap :: Lens' DBusPlugin BusMap
dbusMap inj (DBusPlugin h m) = DBusPlugin h <$> inj m

dbusHost :: Getter DBusPlugin Callbacks
dbusHost inj (DBusPlugin h m) = flip DBusPlugin m <$> inj h

instance Plugin DBusPlugin where
    initialize c = do
        session <- busNew connectSession
        system <- busNew connectSystem
        let busMap = M.fromList [ ("session", session)
                                , ("system", system)
                                ]
        return $ DBusPlugin c busMap

    destroy plugin = forM_ (plugin ^. dbusMap) busDestroy

    handler = dir "dbus" $ msum [ busesHandler
                                , connectBusHandler
                                ]

connectBusHandler :: ServerPart DBusPlugin Response
connectBusHandler = dir "connect" $ do
    nullDir
    name <- look "name"
    addressStr <- look "address"
    address <- MaybeT $ return $ parseAddress addressStr
    bus <- liftIO $ busNew $ connect address
    dbusMap . at name .= Just bus
    okResponse

type BusReference = Lens' DBusPlugin Bus

unsafeMaybeLens :: Lens' (Maybe a) a
unsafeMaybeLens inj (Just v) = Just <$> inj v
unsafeMaybeLens _ Nothing = error "unsafeMaybeLens applied to Nothing"

busesHandler :: ServerPart DBusPlugin Response
busesHandler = path $ \busName -> do
    let busRef :: Lens' DBusPlugin (Maybe Bus)
        busRef = dbusMap . at busName
    bus <- use busRef
    case bus of
      Just _ -> busHandler $ busRef . unsafeMaybeLens
      Nothing -> mzero

busHandler :: BusReference -> ServerPart DBusPlugin Response
busHandler plugin = msum [ listenHandler plugin
                         , stopHandler plugin
                         , callHandler plugin
                         ]

listenHandler :: BusReference -> ServerPart DBusPlugin Response
listenHandler busRef = dir "listen" $ withData $ \matcher -> do
    nullDir
    index <- look "index"
    clnt <- use $ busRef . busClient
    host <- use dbusHost
    listener <- liftIO $ addMatch clnt matcher $ \sig -> callback host index [sig]
    busRef . busSignals . at index .= Just listener
    callbackResponse index

stopHandler :: BusReference -> ServerPart DBusPlugin Response
stopHandler busRef = dir "stop" $ do
    nullDir
    index <- look "index"
    listener <- use (busRef . busSignals . at index)
    case listener of
        Just l -> do
            busRef . busSignals . at index .= Nothing
            clnt <- use $ busRef . busClient
            liftIO $ removeMatch clnt l
            callbackResponse index
        Nothing -> mzero

callHandler :: BusReference -> ServerPart DBusPlugin Response
callHandler busRef = dir "call" $ withData $ \mcall -> do
    nullDir
    clnt <- use $ busRef . busClient
    res <- liftIO $ call clnt mcall
    jsonResponse res
