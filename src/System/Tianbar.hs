module System.Tianbar where

import Control.Monad

import Data.List
import Data.List.Split
import Data.List.Utils

import DBus (toVariant, fromVariant, Signal(..), signal, parseObjectPath, parseInterfaceName, parseMemberName)
import DBus.Client (listen, matchAny, MatchRule(..), connectSession, emit, Client)

import Graphics.UI.Gtk hiding (Signal)
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebSettings

import System.Environment.XDG.BaseDir

import System.Process

import System.Taffybar.Systray
import System.Tianbar.StrutProperties

myScreen = 0
myMonitor = 0
myHeight = 25
mySpacing = 0 -- TODO: is widget spacing needed?

gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) output

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

    on wk resourceRequestStarting $ \_ _ nreq _ -> case nreq of
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

    baseDir <- getUserConfigDir "taffybar"
    htmlFile <- getUserConfigFile "taffybar" "index.html"
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
    postGUIAsync $ do
        webViewExecuteScript wk $ setStatus status

setStatus :: String -> String
setStatus status = let statusStr = escapeQuotes status in
    "window.setXMonadStatus ? window.setXMonadStatus('" ++ statusStr ++ "')" ++
        " : window.XMonadStatus = '" ++ statusStr ++ "'"

xmonadWebkitLogNew :: IO Widget
xmonadWebkitLogNew = do
    l <- webViewNew

    on l realize $ setupWebkitLog l

    Just disp <- displayGetDefault
    screen <- displayGetScreen disp myScreen
    (Rectangle _ _ sw _) <- screenGetMonitorGeometry screen myMonitor
    on l sizeRequest $ return (Requisition (sw `div` 2) myHeight)

    widgetShowAll l
    return (toWidget l)

strutProperties :: Int       -- ^ Bar height
                -> Rectangle -- ^ Current monitor rectangle
                -> StrutProperties
strutProperties bh (Rectangle mX mY mW mH) =
    propertize sX sW sH
    where sX = mX
          sW = mW - 1
          sH = bh + mY
          bottomY (Rectangle _ y _ h) = y + h
          propertize x w h = (0, 0, h, 0, 0, 0, 0, 0, x, x+w, 0, 0)

main = do
    initGUI

    Just disp <- displayGetDefault
    screen <- displayGetScreen disp myScreen
    monitorSize <- screenGetMonitorGeometry screen myMonitor

    window <- windowNew
    widgetSetName window "Tianbar"

    let Rectangle x y w h = monitorSize
    windowSetTypeHint window WindowTypeHintDock
    windowSetScreen window screen
    windowSetDefaultSize window w myHeight
    windowMove window x 0
    onRealize window $
        setStrutProperties window $
            strutProperties myHeight monitorSize

    box <- hBoxNew False mySpacing
    containerAdd window box

    wk <- xmonadWebkitLogNew
    boxPackStart box wk PackGrow 0

    tray <- systrayNew
    boxPackEnd box tray PackNatural 0

    widgetShow window
    widgetShow box

    mainGUI
    return ()
