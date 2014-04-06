{-# Language OverloadedStrings #-}
module System.Tianbar.DBus where

-- DBus connectivity

import Control.Monad

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

    handleRequest dbus = withScheme "dbus:" $ \uri -> do
        let busCall = splitOn "/" (uriPath uri)
        let params = parseQuery uri
        case busCall of
            [bus, "listen"] -> do
                let client = uriBus bus dbus
                dbusListen (dbusHost dbus) client params
                returnContent ""
            [bus, "call"] -> do
                let client = uriBus bus dbus
                result <- dbusCall client params
                returnJSON result
            _ -> returnContent ""

dbusListen :: WebView -> Client -> URIParams -> IO ()
dbusListen wk client params = do
    let matcher = matchRuleUri params
    let (Just index) = lookupQueryParam "index" params
    _ <- addMatch client matcher $ \sig -> callback wk index [sig]
    return ()

dbusCall :: Client -> URIParams -> IO (Either MethodError MethodReturn)
dbusCall client params = do
    let mcall = methodCallUri params
    call client mcall

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

methodCallUri :: URIParams -> MethodCall
methodCallUri params = (methodCall callPath iface member) { methodCallBody = body
                                                          , methodCallDestination = dest
                                                          }
    where (Just callPath) = parseObjectPath $ getQueryParam "path" params
          (Just iface) = parseInterfaceName $ getQueryParam "iface" params
          (Just member) = parseMemberName $ getQueryParam "member" params
          body = map variantFromString $ getQueryParams "body[]" params
          dest = lookupQueryParam "destination" params >>= parseBusName
