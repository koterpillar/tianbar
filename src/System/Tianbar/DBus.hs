{-# Language OverloadedStrings #-}
module System.Tianbar.DBus where

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

-- DBus callback override

type DBusClient = MVar Client

data DBusState = DBusState { dbusSession :: DBusClient
                           , dbusSystem :: DBusClient
                           }

initDBusState :: IO DBusState
initDBusState = do
    session <- connectSession >>= newMVar
    system <- connectSystem >>= newMVar
    return $ DBusState session system

reloadDBusState :: DBusState -> IO ()
reloadDBusState dbus = do
    modifyMVar_ (dbusSession dbus) $ \client -> do
        disconnect client
        connectSession
    modifyMVar_ (dbusSystem dbus) $ \client -> do
        disconnect client
        connectSystem
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
dbusOverride wk dbus = withScheme "dbus:" $ \uri -> do
    let [busName, busCall] = splitOn "/" (uriPath uri)
    let bus = uriBus busName dbus
    withMVar bus $ \client -> case busCall of
        "listen" -> do
            dbusListen wk client uri
            returnContent ""
        "call" -> do
            result <- dbusCall wk client uri
            returnJSON result
        _ -> returnContent ""

dbusListen :: WebView -> Client -> URI -> IO ()
dbusListen wk client uri = do
    let params = parseQuery uri
    let matcher = matchRuleUri uri
    let (Just index) = liftM read $ lookupQueryParam "index" params
    _ <- addMatch client matcher $ callback wk index
    return ()

dbusCall :: WebView -> Client -> URI -> IO (Either MethodError MethodReturn)
dbusCall _ client uri = do
    let mcall = methodCallUri uri
    call client mcall

uriBus :: String -> DBusState -> MVar Client
uriBus "session" = dbusSession
uriBus "system" = dbusSystem
uriBus _ = error "Unknown bus"

matchRuleUri :: URI -> MatchRule
matchRuleUri uri = matchAny { matchSender = Nothing
                            , matchDestination = Nothing
                            , matchPath = lookupQueryParam "path" params >>= parseObjectPath
                            , matchInterface = lookupQueryParam "iface" params >>= parseInterfaceName
                            , matchMember = lookupQueryParam "member" params >>= parseMemberName
                            }
    where params = parseQuery uri

variantFromString :: String -> Variant
variantFromString param = case splitOn ":" param of
    ["string", str] -> toVariant str
    _ -> error "Invalid variant string"

methodCallUri :: URI -> MethodCall
methodCallUri uri = (methodCall callPath iface member) { methodCallBody = body
                                                       , methodCallDestination = dest
                                                       }
    where params = parseQuery uri
          (Just callPath) = parseObjectPath $ getQueryParam "path" params
          (Just iface) = parseInterfaceName $ getQueryParam "iface" params
          (Just member) = parseMemberName $ getQueryParam "member" params
          body = map variantFromString $ getQueryParams "body[]" params
          dest = lookupQueryParam "destination" params >>= parseBusName
