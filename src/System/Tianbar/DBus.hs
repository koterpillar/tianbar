{-# Language OverloadedStrings #-}
module System.Tianbar.DBus where

import Control.Concurrent.MVar
import Control.Monad

import Data.Aeson
import Data.Int
import Data.List
import qualified Data.Map as M
import qualified Data.Maybe as MA
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Word

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.WebKit.WebView

import DBus
import DBus.Client

import Network.URI

import System.Tianbar.UriOverride

-- DBus callback override
data DBusState = DBusState { dbusClient :: MVar Client
                           }

instance ToJSON Variant where
    toJSON v = let t = variantType v in
        case t of
            TypeBoolean -> let Just b = fromVariant v :: Maybe Bool in toJSON b
            TypeString -> let Just s = fromVariant v :: Maybe String in toJSON s
            TypeWord8 -> let Just i = fromVariant v :: Maybe Word8 in toJSON i
            TypeWord16 -> let Just i = fromVariant v :: Maybe Word16 in toJSON i
            TypeWord32 -> let Just i = fromVariant v :: Maybe Word32 in toJSON i
            TypeWord64 -> let Just i = fromVariant v :: Maybe Word64 in toJSON i
            TypeInt16 -> let Just i = fromVariant v :: Maybe Int16 in toJSON i
            TypeInt32 -> let Just i = fromVariant v :: Maybe Int32 in toJSON i
            TypeInt64 -> let Just i = fromVariant v :: Maybe Int64 in toJSON i
            -- TODO: more types
            _ -> error $ "Variant type not supported: "
                      ++ show v ++ " (type: " ++ show t ++ ")"

instance ToJSON ObjectPath where
    toJSON = toJSON . formatObjectPath

instance ToJSON InterfaceName where
    toJSON = toJSON . formatInterfaceName

instance ToJSON MemberName where
    toJSON = toJSON . formatMemberName

instance ToJSON Signal where
    toJSON s = object [ "path"   .= toJSON (signalPath s)
                      , "iface"  .= toJSON (signalInterface s)
                      , "member" .= toJSON (signalMember s)
                      ]

instance ToJSON ErrorName where
    toJSON = toJSON . formatErrorName

instance ToJSON Serial where
    toJSON = toJSON . serialValue

instance ToJSON BusName where
    toJSON = toJSON . formatBusName

instance ToJSON MethodError where
    toJSON e = object [ "name"        .= toJSON (methodErrorName e)
                      , "serial"      .= toJSON (methodErrorSerial e)
                      , "sender"      .= toJSON (methodErrorSender e)
                      , "destination" .= toJSON (methodErrorDestination e)
                      , "body"        .= toJSON (methodErrorBody e)
                      , "message"     .= toJSON (methodErrorMessage e)
                      ]

instance ToJSON MethodReturn where
    toJSON r = object [ "serial"      .= toJSON (methodReturnSerial r)
                      , "sender"      .= toJSON (methodReturnSender r)
                      , "destination" .= toJSON (methodReturnDestination r)
                      , "body"        .= toJSON (methodReturnBody r)
                      ]

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
dbusOverride wk dbus = withScheme "dbus:" $ \uri ->
    case uriPath uri of
        "listen" -> do
            dbusListen wk dbus uri
            returnContent ""
        "call" -> do
            result <- dbusCall wk dbus uri
            returnJSON result
        _ -> returnContent ""

dbusListen :: WebView -> DBusState -> URI -> IO ()
dbusListen wk dbus uri = do
    let params = parseQuery uri
    let matcher = matchAny { matchSender = Nothing
                           , matchDestination = Nothing
                           , matchPath = M.lookup "path" params >>= parseObjectPath
                           , matchInterface = M.lookup "iface" params >>= parseInterfaceName
                           , matchMember = M.lookup "member" params >>= parseMemberName
                           }
    let (Just index) = liftM read $ M.lookup "index" params
    withMVar (dbusClient dbus) $ \client -> do
        _ <- addMatch client matcher $ callback wk index
        return ()

dbusCall :: WebView -> DBusState -> URI -> IO (Either MethodError MethodReturn)
dbusCall _ dbus uri = do
    print uri
    let params = parseQuery uri
    let path = MA.fromJust $ M.lookup "path" params >>= parseObjectPath
    let iface = MA.fromJust $ M.lookup "iface" params >>= parseInterfaceName
    let member = MA.fromJust $ M.lookup "member" params >>= parseMemberName
    let mcall = (methodCall path iface member) { methodCallBody = [toVariant $ MA.fromJust $ M.lookup "body" params]
        , methodCallDestination = M.lookup "destination" params >>= parseBusName
        }
    withMVar (dbusClient dbus) $ flip call mcall
