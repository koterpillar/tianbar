{-# Language OverloadedStrings #-}
module System.Tianbar.WebKit where

import Control.Concurrent.MVar
import Control.Monad

import Data.List.Split

-- TODO: Move state to DBus override
import DBus.Client (connectSession, disconnect)

import Graphics.UI.Gtk hiding (disconnect, Signal, Variant)
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebWindowFeatures

import Network.URI

import System.Environment.XDG.BaseDir

import System.Process

import System.Tianbar.Configuration
import System.Tianbar.DBus
import System.Tianbar.UriOverride

import Paths_tianbar

-- GSettings URI override
gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) output

gsettingsUriOverride :: UriOverride
gsettingsUriOverride = withScheme "gsettings:" $ \uri -> do
    let [schema, key] = splitOn "/" $ uriPath uri
    setting <- gsettingsGet schema key
    returnContent setting

-- Data directory override
dataFileOverride :: UriOverride
dataFileOverride = withScheme "tianbar:" $ \uri -> do
    liftM ("file://" ++) $ getDataFileName $ uriPath uri

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
