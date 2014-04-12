{-# Language OverloadedStrings #-}
module System.Tianbar.DBus where

-- DBus connectivity

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Data.List.Split

import Graphics.UI.Gtk.WebKit.WebView

import DBus
import DBus.Client

import Network.URI

import System.Tianbar.DBus.JSON ()
import System.Tianbar.Plugin

data DBusPlugin = DBusPlugin { dbusHost :: WebView
                             , dbusSession :: Client
                             , dbusSystem :: Client
                             }

instance Plugin DBusPlugin where
    initialize wk = do
        session <- connectSession
        system <- connectSystem
        return $ DBusPlugin wk session system

    destroy dbus = mapM_ disconnect [dbusSession dbus, dbusSystem dbus]

    handleRequest dbus = withScheme "dbus:" $ \uri -> runMaybeT $ do
        let busCall = splitOn "/" (uriPath uri)
        let params = parseQuery uri
        case busCall of
            [bus, "listen"] -> do
                let client = uriBus bus dbus
                _ <- dbusListen (dbusHost dbus) client params
                returnContent "ok"
            [bus, "call"] -> do
                let client = uriBus bus dbus
                result <- dbusCall client params
                returnJSON result
            _ -> error "Invalid call"

dbusListen :: WebView -> Client -> URIParams -> MaybeT IO SignalHandler
dbusListen wk client params = do
    let matcher = matchRuleUri params
    index <- liftMT $ lookupQueryParam "index" params
    liftIO $ addMatch client matcher $ \sig -> callback wk index [sig]

dbusCall :: Client -> URIParams -> MaybeT IO (Either MethodError MethodReturn)
dbusCall client params = do
    mcall <- liftMT $ methodCallUri params
    liftIO $ call client mcall

uriBus :: String -> DBusPlugin -> Client
uriBus "session" = dbusSession
uriBus "system" = dbusSystem
uriBus _ = error "Unknown bus"

matchRuleUri :: URIParams -> MatchRule
matchRuleUri params = matchAny { matchSender = Nothing
                               , matchDestination = Nothing
                               , matchPath = lookupQueryParam "path" params >>= parseObjectPath
                               , matchInterface = lookupQueryParam "iface" params >>= parseInterfaceName
                               , matchMember = lookupQueryParam "member" params >>= parseMemberName
                               }

variantFromString :: String -> Variant
variantFromString param = case splitOn ":" param of
    ["string", str] -> toVariant str
    _ -> error "Invalid variant string"

methodCallUri :: URIParams -> Maybe MethodCall
methodCallUri params = liftM setBodyDest $ methodCall <$> callPath <*> iface <*> member
    where callPath = lookupQueryParam "path" params >>= parseObjectPath
          iface = lookupQueryParam "iface" params >>= parseInterfaceName
          member = lookupQueryParam "member" params >>= parseMemberName
          setBodyDest mcall = mcall { methodCallBody = body
                                    , methodCallDestination = dest
                                    }
          body = map variantFromString $ getQueryParams "body[]" params
          dest = lookupQueryParam "destination" params >>= parseBusName
