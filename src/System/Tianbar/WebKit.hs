module System.Tianbar.WebKit where

import Control.Monad

import Data.List
import Data.List.Split
import Data.List.Utils

import DBus (fromVariant, Signal(..), parseObjectPath, parseInterfaceName, parseMemberName)
import DBus.Client (listen, matchAny, MatchRule(..), connectSession)

import Graphics.UI.Gtk hiding (Signal)
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebWindowFeatures

import System.Environment.XDG.BaseDir

import System.Process

import System.Tianbar.Configuration

import Paths_tianbar

gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) output

type UriOverride = String -> Maybe (IO String)

withPrefix :: String -> (String -> IO String) -> UriOverride
withPrefix prefix func uri
    | prefix `isPrefixOf` uri = Just $ func $ drop (length prefix) uri
    | otherwise = Nothing

gsettingsUriOverride :: UriOverride
gsettingsUriOverride = withPrefix "gsettings:" $ \path -> do
    let [schema, key] = splitOn "/" path
    setting <- gsettingsGet schema key
    return $ "data:text/plain," ++ setting

dataFileOverride :: UriOverride
dataFileOverride = withPrefix "tianbar:" $ \path ->
    liftM ("file://" ++) $ getDataFileName path

uriOverrides :: [UriOverride]
uriOverrides = [gsettingsUriOverride, dataFileOverride]

allOverrides :: UriOverride
allOverrides = foldr mplus Nothing . flip map uriOverrides . flip ($)

tianbarWebView :: IO WebView
tianbarWebView = do
    wk <- webViewNew

    -- Enable AJAX access to all domains
    wsettings <- webViewGetWebSettings wk
    set wsettings [webSettingsEnableUniversalAccessFromFileUris := True]
    webViewSetWebSettings wk wsettings

    -- Process the special overrides
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


setupWebkitLog :: WebView -> IO ()
setupWebkitLog wk = do
    let matcher = matchAny { matchSender = Nothing
                           , matchDestination = Nothing
                           , matchPath = parseObjectPath "/org/xmonad/Log"
                           , matchInterface = parseInterfaceName "org.xmonad.Log"
                           , matchMember = parseMemberName "Update"
                           }

    htmlFile <- getUserConfigFile appName "index.html"
    html <- readFile htmlFile
    webViewLoadHtmlString wk html $ "file://" ++ htmlFile

    client <- connectSession

    listen client matcher $ callback wk

escapeQuotes :: String -> String
escapeQuotes = replace "'" "\\'" . replace "\\" "\\\\"

callback :: WebView -> Signal -> IO ()
callback wk sig = do
    let [bdy] = signalBody sig
        Just status = fromVariant bdy
    postGUIAsync $ webViewExecuteScript wk $ setStatus status

setStatus :: String -> String
setStatus status = let statusStr = escapeQuotes status in
    "window.setXMonadStatus ? window.setXMonadStatus('" ++ statusStr ++ "')" ++
        " : window.XMonadStatus = '" ++ statusStr ++ "'"

xmonadWebkitLogNew :: IO Widget
xmonadWebkitLogNew = do
    l <- tianbarWebView

    _ <- on l realize $ setupWebkitLog l

    Just disp <- displayGetDefault
    screen <- displayGetScreen disp myScreen
    (Rectangle _ _ sw _) <- screenGetMonitorGeometry screen myMonitor
    _ <- on l sizeRequest $ return (Requisition (sw `div` 2) barHeight)

    widgetShowAll l
    return (toWidget l)
