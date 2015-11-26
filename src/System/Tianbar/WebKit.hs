{-# Language OverloadedStrings #-}
module System.Tianbar.WebKit where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import GI.Gtk hiding (main)
import qualified GI.Gtk as Gtk
import GI.GtkSignals ()
import qualified GI.GLib as GLib
import GI.WebKit
import GI.WebKitSignals ()
import GI.WebKitAttributes ()

-- import GI.Properties
-- import GI.Signals
import Data.GI.Base

import Network.URI

import System.Directory
import System.Environment.XDG.BaseDir

import System.Tianbar.Callbacks
import System.Tianbar.Configuration
import System.Tianbar.Server
import System.Tianbar.Utils

import Paths_tianbar

tianbarWebView :: IO WebView
tianbarWebView = do
    wk <- new WebView []

    -- Enable AJAX access to all domains
    wsettings <- webViewGetWebSettings wk
    set wsettings [webSettingsEnableUniversalAccessFromFileUris := True]
    webViewSetWebSettings wk wsettings

    -- Enable geolocation
    _ <- on wk geolocationPolicyDecisionRequested $ \_ decision -> do
        geolocationPolicyAllow decision
        return True

    -- Initialize plugins, and re-initialize on reloads
    server <- startServer (callbacks wk) >>= newMVar
    _ <- on wk loadStarted $ \_ -> modifyMVar_ server $ \oldServer -> do
        stopServer oldServer
        startServer (callbacks wk)

    -- Process the special overrides
    _ <- on wk resourceRequestStarting $ \_ _ nreq _ -> void $ runMaybeT $ do
        req <- liftMT nreq
        uriStr <- MaybeT $ networkRequestGetUri req
        uri <- liftMT $ parseURI uriStr
        override <- liftIO $ withMVar server $ return . serverOverrideURI
        let uri' = override uri
        liftIO $ networkRequestSetUri req $ show uri'

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

    -- If the file does not exist, copy an example file over
    exists <- doesFileExist htmlFile
    unless exists $ do
        -- Ensure config directory exists
        getUserConfigDir appName >>= createDirectoryIfMissing True
        exampleHtml <- getDataFileName "index.html"
        copyFile exampleHtml htmlFile

    webViewLoadUri wk $ "file://" ++ htmlFile

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
