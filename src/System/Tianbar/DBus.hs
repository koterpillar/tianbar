{-# Language OverloadedStrings #-}
module System.Tianbar.DBus where

-- DBus connectivity

import Control.Concurrent.MVar
import Control.Monad

import Data.Aeson hiding (Array)
import Data.List
import Data.List.Split
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.WebKit.WebView

import DBus
import DBus.Client

import Network.URI

import System.Tianbar.DBus.JSON ()
import System.Tianbar.UriOverride

type DBusState = MVar DBusConn

data DBusConn = DBusConn { dbusSession :: Client
                         , dbusSystem :: Client
                         }

connectState :: IO DBusConn
connectState = do
    session <- connectSession
    system <- connectSystem
    return $ DBusConn session system

initDBusState :: IO DBusState
initDBusState = connectState >>= newMVar

reloadDBusState :: DBusState -> IO ()
reloadDBusState state = modifyMVar_ state $ \dbus -> do
    mapM_ disconnect [dbusSession dbus, dbusSystem dbus]
    connectState

callback :: WebView -> Int -> Signal -> IO ()
callback wk index sig =
    Gtk.postGUIAsync $ webViewExecuteScript wk $ dbusCallback index sig

dbusCallback :: Int -> Signal -> String
dbusCallback index sig =
    "window.dbusCallbacks && "
        ++ "window.dbusCallbacks[" ++ indexStr ++ "](" ++ paramsStr ++ ")"
    where indexStr = show index
          paramsStr = intercalate "," $ map (T.unpack . E.decodeUtf8) [signalStr, bodyStr]
          signalStr = encode $ toJSON sig
          bodyStr = encode $ toJSON $ signalBody sig

returnJSON :: (ToJSON a) => a -> IO String
returnJSON = returnContent . T.unpack . E.decodeUtf8 . encode . toJSON

dbusOverride :: WebView -> DBusState -> UriOverride
dbusOverride wk state = withScheme "dbus:" $ \uri -> do
    let busCall = splitOn "/" (uriPath uri)
    let params = parseQuery uri
    case busCall of
        [bus, "listen"] -> withMVar state $ \dbus -> do
            let client = uriBus bus dbus
            dbusListen wk client params
            returnContent ""
        [bus, "call"] -> withMVar state $ \dbus -> do
            let client = uriBus bus dbus
            result <- dbusCall client params
            returnJSON result
        _ -> returnContent ""

dbusListen :: WebView -> Client -> URIParams -> IO ()
dbusListen wk client params = do
    let matcher = matchRuleUri params
    let (Just index) = liftM read $ lookupQueryParam "index" params
    _ <- addMatch client matcher $ callback wk index
    return ()

dbusCall :: Client -> URIParams -> IO (Either MethodError MethodReturn)
dbusCall client params = do
    let mcall = methodCallUri params
    call client mcall

uriBus :: String -> DBusConn -> Client
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
