module System.Tianbar.WebKit where

import Control.Monad

import Data.List
import Data.List.Split
import Data.List.Utils
import qualified Data.Map as M

import DBus (fromVariant, Signal(..), parseObjectPath, parseInterfaceName, parseMemberName)
import DBus.Client (Client, listen, matchAny, MatchRule(..), connectSession)

import Graphics.UI.Gtk hiding (Signal)
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
escapeQuotes :: String -> String
escapeQuotes = replace "'" "\\'" . replace "\\" "\\\\"

callback :: WebView -> Int -> Signal -> IO ()
callback wk index sig = do
    let [bdy] = signalBody sig
        Just result = fromVariant bdy
    postGUIAsync $ webViewExecuteScript wk $ dbusCallback index result

dbusCallback :: Int -> String -> String
dbusCallback index result =
    "window.dbusCallbacks[" ++ indexStr ++ "]('" ++ resultStr ++ "')"
    where indexStr = show index
          resultStr = escapeQuotes result

dbusOverride :: WebView -> Client -> UriOverride
dbusOverride wk dbus = withPrefix "dbus:" $ \path -> do
    let params = parseQuery path
    let matcher = matchAny { matchSender = Nothing
                           , matchDestination = Nothing
                           , matchPath = M.lookup "path" params >>= parseObjectPath
                           , matchInterface = M.lookup "iface" params >>= parseInterfaceName
                           , matchMember = M.lookup "member" params >>= parseMemberName
                           }
    let (Just index) = M.lookup "index" params >>= (return . read)
    listen dbus matcher $ callback wk index
    returnContent ""

mergeOverrides :: [UriOverride] -> UriOverride
mergeOverrides overrides = foldr mplus Nothing . flip map overrides . flip ($)

tianbarWebView :: Client -> IO WebView
tianbarWebView dbus = do
    wk <- webViewNew

    -- Enable AJAX access to all domains
    wsettings <- webViewGetWebSettings wk
    set wsettings [webSettingsEnableUniversalAccessFromFileUris := True]
    webViewSetWebSettings wk wsettings

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
        nwk <- tianbarWebView dbus

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
    dbus <- connectSession

    l <- tianbarWebView dbus

    _ <- on l realize $ loadIndexPage l

    Just disp <- displayGetDefault
    screen <- displayGetScreen disp myScreen
    (Rectangle _ _ sw _) <- screenGetMonitorGeometry screen myMonitor
    _ <- on l sizeRequest $ return (Requisition (sw `div` 2) barHeight)

    widgetShowAll l
    return (toWidget l)
