{-# Language OverloadedStrings #-}
module System.Tianbar.WebKit where

import Control.Concurrent.MVar
import Control.Monad

import Data.Aeson
import Data.Int
import Data.List
import Data.List.Split
import Data.List.Utils
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Word

import DBus
import DBus.Client (Client, listen, matchAny, MatchRule(..), connectSession, disconnect)

import Graphics.UI.Gtk hiding (disconnect, Signal, Variant)
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebWindowFeatures

import Network.URI (unEscapeString)

import System.Environment.XDG.BaseDir

import System.Process

import System.Tianbar.Configuration

import Paths_tianbar

type UriOverride = String -> Maybe (IO String)

withPrefix :: String -> (String -> IO String) -> UriOverride
withPrefix prefix func uri
    | prefix `isPrefixOf` uri = Just $ func $ drop (length prefix) uri
    | otherwise = Nothing

returnContent :: String -> IO String
returnContent content = return $ "data:text/plain," ++ content

parseQuery :: String -> M.Map String String
parseQuery = M.fromList
           . map ((\[a, b] -> (a, unEscapeString b)) . splitOn "=")
           . splitOn "&"
           . tail
           .dropWhile (/= '?')

-- GSettings URI override
gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) output

gsettingsUriOverride :: UriOverride
gsettingsUriOverride = withPrefix "gsettings:" $ \path -> do
    let [schema, key] = splitOn "/" path
    setting <- gsettingsGet schema key
    returnContent setting

-- Data directory override
dataFileOverride :: UriOverride
dataFileOverride = withPrefix "tianbar:" $ \path -> do
    liftM ("file://" ++) $ getDataFileName path

-- DBus callback override
data DBusState = DBusState { dbusClient :: MVar Client
                           }

escapeQuotes :: String -> String
escapeQuotes = replace "'" "\\'" . replace "\\" "\\\\"

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
    postGUIAsync $ webViewExecuteScript wk $ dbusCallback index sig

dbusCallback :: Int -> Signal -> String
dbusCallback index sig =
    "window.dbusCallbacks && "
        ++ "window.dbusCallbacks[" ++ indexStr ++ "](" ++ paramsStr ++ ")"
    where indexStr = show index
          paramsStr = intercalate "," $ map (T.unpack . E.decodeUtf8) [signalStr, bodyStr]
          signalStr = encode $ toJSON sig
          bodyStr = encode $ toJSON $ signalBody sig

dbusOverride :: WebView -> DBusState -> UriOverride
dbusOverride wk dbus = withPrefix "dbus:" $ \path -> do
    let params = parseQuery path
    let matcher = matchAny { matchSender = Nothing
                           , matchDestination = Nothing
                           , matchPath = M.lookup "path" params >>= parseObjectPath
                           , matchInterface = M.lookup "iface" params >>= parseInterfaceName
                           , matchMember = M.lookup "member" params >>= parseMemberName
                           }
    let (Just index) = M.lookup "index" params >>= (return . read)
    withMVar (dbusClient dbus) $ \client ->
        listen client matcher $ callback wk index
    returnContent ""

mergeOverrides :: [UriOverride] -> UriOverride
mergeOverrides overrides = foldr mplus Nothing . flip map overrides . flip ($)

tianbarWebView :: IO WebView
tianbarWebView = do
    wk <- webViewNew

    -- Enable AJAX access to all domains
    wsettings <- webViewGetWebSettings wk
    set wsettings [webSettingsEnableUniversalAccessFromFileUris := True]
    webViewSetWebSettings wk wsettings

    -- Connect DBus listener, and reconnect on reloads
    dbus <- liftM DBusState $ connectSession >>= newMVar

    _ <- on wk loadStarted $ \_ -> do
        modifyMVar_ (dbusClient dbus) $ \client -> do
            disconnect client
            connectSession

    -- Process the special overrides
    let allOverrides = mergeOverrides [ gsettingsUriOverride
                                      , dataFileOverride
                                      , dbusOverride wk dbus
                                      ]
    _ <- on wk resourceRequestStarting $ \_ _ nreq _ -> case nreq of
        Nothing -> return ()
        (Just req) -> do
            uri <- networkRequestGetUri req
            let override_ = uri >>= allOverrides
            case override_ of
                Nothing -> return ()
                (Just override) -> override >>= networkRequestSetUri req

    -- Handle new window creation
    _ <- on wk createWebView $ \_ -> do
        nwk <- tianbarWebView

        window <- windowNew
        containerAdd window nwk

        _ <- on nwk webViewReady $ do
            wfeat <- webViewGetWindowFeatures nwk

            [wx, wy, ww, wh] <- mapM (get wfeat) [ webWindowFeaturesX
                                                 , webWindowFeaturesY
                                                 , webWindowFeaturesWidth
                                                 , webWindowFeaturesHeight
                                                 ]

            windowSetGeometryHints window
                                       (Nothing :: Maybe Window)
                                       (Just (ww, wh))
                                       (Just (ww, wh))
                                       Nothing
                                       Nothing
                                       Nothing

            widgetShow window
            widgetShow nwk

            windowMove window wx wy
            windowSetKeepAbove window True
            windowStick window

            return False

        return nwk

    return wk


loadIndexPage :: WebView -> IO ()
loadIndexPage wk = do
    htmlFile <- getUserConfigFile appName "index.html"
    html <- readFile htmlFile
    webViewLoadHtmlString wk html $ "file://" ++ htmlFile

tianbarWebkitNew :: IO Widget
tianbarWebkitNew = do
    l <- tianbarWebView

    _ <- on l realize $ loadIndexPage l

    Just disp <- displayGetDefault
    screen <- displayGetScreen disp myScreen
    (Rectangle _ _ sw _) <- screenGetMonitorGeometry screen myMonitor
    _ <- on l sizeRequest $ return (Requisition (sw `div` 2) barHeight)

    widgetShowAll l
    return (toWidget l)
