{-# Language OverloadedStrings #-}
module System.Tianbar.DBus where

import Control.Concurrent.MVar

import Data.Aeson
import Data.Int
import Data.List
import qualified Data.Map as M
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

instance ToJSON Signal where
    toJSON s = object [ "path"   .= toJSON (formatObjectPath $ signalPath s)
                      , "iface"  .= toJSON (formatInterfaceName $ signalInterface s)
                      , "member" .= toJSON (formatMemberName $ signalMember s)
                      ]

callback :: WebView -> Int -> Signal -> IO ()
callback wk index sig = do
    Gtk.postGUIAsync $ webViewExecuteScript wk $ dbusCallback index sig

dbusCallback :: Int -> Signal -> String
dbusCallback index sig =
    "window.dbusCallbacks && "
        ++ "window.dbusCallbacks[" ++ indexStr ++ "](" ++ paramsStr ++ ")"
    where indexStr = show index
          paramsStr = intercalate "," $ map (T.unpack . E.decodeUtf8) [signalStr, bodyStr]
          signalStr = encode $ toJSON sig
          bodyStr = encode $ toJSON $ signalBody sig

dbusOverride :: WebView -> DBusState -> UriOverride
dbusOverride wk dbus = withScheme "dbus:" $ \uri -> do
    case (uriPath uri) of
        "listen" -> dbusListen wk dbus uri
        _ -> return ()
    returnContent ""


dbusListen :: WebView -> DBusState -> URI -> IO ()
dbusListen wk dbus uri = do
    let params = parseQuery uri
    let matcher = matchAny { matchSender = Nothing
                           , matchDestination = Nothing
                           , matchPath = M.lookup "path" params >>= parseObjectPath
                           , matchInterface = M.lookup "iface" params >>= parseInterfaceName
                           , matchMember = M.lookup "member" params >>= parseMemberName
                           }
    let (Just index) = M.lookup "index" params >>= (return . read)
    withMVar (dbusClient dbus) $ \client ->
        listen client matcher $ callback wk index
