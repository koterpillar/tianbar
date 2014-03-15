{-# Language OverloadedStrings #-}
module System.Tianbar.DBus where

import Control.Concurrent.MVar
import Control.Monad

import Data.Aeson hiding (Array)
import Data.Int
import Data.List
import Data.List.Split
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

instance ToJSON Variant where
    toJSON v = case variantType v of
        TypeBoolean -> let Just b = fromVariant v :: Maybe Bool in toJSON b
        TypeWord8 -> let Just i = fromVariant v :: Maybe Word8 in toJSON i
        TypeWord16 -> let Just i = fromVariant v :: Maybe Word16 in toJSON i
        TypeWord32 -> let Just i = fromVariant v :: Maybe Word32 in toJSON i
        TypeWord64 -> let Just i = fromVariant v :: Maybe Word64 in toJSON i
        TypeInt16 -> let Just i = fromVariant v :: Maybe Int16 in toJSON i
        TypeInt32 -> let Just i = fromVariant v :: Maybe Int32 in toJSON i
        TypeInt64 -> let Just i = fromVariant v :: Maybe Int64 in toJSON i
        TypeDouble -> let Just i = fromVariant v :: Maybe Double in toJSON i
        TypeString -> let Just s = fromVariant v :: Maybe String in toJSON s

        TypeSignature -> let Just s = fromVariant v :: Maybe Signature in
            toJSON $ formatSignature s
        TypeObjectPath -> let Just p = fromVariant v :: Maybe ObjectPath in
            toJSON $ formatObjectPath p

        TypeVariant -> let Just n = fromVariant v :: Maybe Variant in toJSON n
        TypeArray _ -> let Just a = fromVariant v :: Maybe Array in
            toJSON $ arrayItems a
        TypeDictionary TypeString _ -> let Just d = fromVariant v :: Maybe Dictionary in
            toJSON $ M.fromList $ map variantStringKey $ dictionaryItems d
        TypeStructure _ -> let Just a = fromVariant v :: Maybe Structure in
            toJSON $ structureItems a

variantString :: Variant -> String
variantString v = case variantType v of
    TypeString -> fromVariant v
    TypeVariant -> fromVariant v >>= variantString
    _ -> show v

variantStringKey :: (Variant, Variant) -> (String, Variant)
variantStringKey (k, v) = (variantString k, v)

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
