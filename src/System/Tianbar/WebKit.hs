module System.Tianbar.WebKit where

import Control.Monad

import Data.List
import Data.List.Split
import Data.List.Utils

import DBus (fromVariant, Signal(..), parseObjectPath, parseInterfaceName, parseMemberName)
import DBus.Client (listen, matchAny, MatchRule(..), connectSession)

import Graphics.UI.Gtk hiding (Signal)
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings

import System.Environment.XDG.BaseDir

import System.Process

import System.Tianbar.Configuration

gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) output

gsettingsPrefix :: String
gsettingsPrefix = "gsettings:"

setupWebkitLog :: WebView -> IO ()
setupWebkitLog wk = do
    let matcher = matchAny { matchSender = Nothing
                           , matchDestination = Nothing
                           , matchPath = parseObjectPath "/org/xmonad/Log"
                           , matchInterface = parseInterfaceName "org.xmonad.Log"
                           , matchMember = parseMemberName "Update"
                           }

    wsettings <- webViewGetWebSettings wk
    set wsettings [webSettingsEnableUniversalAccessFromFileUris := True]
    webViewSetWebSettings wk wsettings

    _ <- on wk resourceRequestStarting $ \_ _ nreq _ -> case nreq of
        Nothing -> return ()
        (Just req) -> do
            uri_ <- networkRequestGetUri req
            case uri_ of
                Nothing -> return ()
                Just uri -> when (gsettingsPrefix `isPrefixOf` uri) $ do
                    let path = drop (length gsettingsPrefix) uri
                    let [schema, key] = splitOn "/" path
                    setting <- gsettingsGet schema key
                    networkRequestSetUri req $
                        "data:text/plain," ++ setting

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
    l <- webViewNew

    _ <- on l realize $ setupWebkitLog l

    Just disp <- displayGetDefault
    screen <- displayGetScreen disp myScreen
    (Rectangle _ _ sw _) <- screenGetMonitorGeometry screen myMonitor
    _ <- on l sizeRequest $ return (Requisition (sw `div` 2) barHeight)

    widgetShowAll l
    return (toWidget l)
