{-# Language OverloadedStrings #-}
module System.Tianbar.WebKit where

import Control.Concurrent.MVar (modifyMVar_, newMVar, withMVar)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Data.List.Split

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
import System.Tianbar.Socket
import System.Tianbar.Plugin
import System.Tianbar.Plugin.Combined

import Paths_tianbar

-- GSettings plugin
data GSettings = GSettings

instance Plugin GSettings where
    initialize _ = return GSettings
    handleRequest _ = withScheme "gsettings:" $ \uri -> do
        let [schema, key] = splitOn "/" $ uriPath uri
        setting <- gsettingsGet schema key
        return $ Just $ plainContent setting

gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) output

-- Data directory override
data DataDirectory = DataDirectory

instance Plugin DataDirectory where
    initialize _ = return DataDirectory
    handleRequest _ = withScheme "tianbar:" $ \uri -> do
        let path = uriPath uri
        dataFile <- getDataFileName path
        return $ Just $ "file://" ++ dataFile

type AllPlugins = Combined GSettings (
                  Combined DataDirectory (
                  Combined SocketPlugin (
                  Combined DBusPlugin
                  Empty)))

tianbarWebView :: IO WebView
tianbarWebView = do
    wk <- webViewNew

    -- Enable AJAX access to all domains
    wsettings <- webViewGetWebSettings wk
    set wsettings [webSettingsEnableUniversalAccessFromFileUris := True]
    webViewSetWebSettings wk wsettings

    -- Initialize plugins, and re-initialize on reloads
    plugins <- (initialize wk :: IO AllPlugins) >>= newMVar
    _ <- on wk loadStarted $ \_ -> modifyMVar_ plugins $ \oldPlugins -> do
        destroy oldPlugins
        initialize wk

    -- Process the special overrides
    _ <- on wk resourceRequestStarting $ \_ _ nreq _ -> void $ runMaybeT $ do
        req <- liftMT nreq
        uri <- MaybeT $ networkRequestGetUri req
        override <- MaybeT $ withMVar plugins $ flip handleRequest uri
        liftIO $ networkRequestSetUri req override

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
