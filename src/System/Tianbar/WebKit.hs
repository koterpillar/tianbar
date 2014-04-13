{-# Language OverloadedStrings #-}
module System.Tianbar.WebKit where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Graphics.UI.Gtk hiding (disconnect, Signal, Variant)
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebWindowFeatures

import Network.URI

import System.Environment.XDG.BaseDir

import System.Tianbar.Configuration
import System.Tianbar.Server
import System.Tianbar.Utils

tianbarWebView :: IO WebView
tianbarWebView = do
    wk <- webViewNew

    -- Enable AJAX access to all domains
    wsettings <- webViewGetWebSettings wk
    set wsettings [webSettingsEnableUniversalAccessFromFileUris := True]
    webViewSetWebSettings wk wsettings

    -- Initialize plugins, and re-initialize on reloads
    server <- startServer wk >>= newMVar
    _ <- on wk loadStarted $ \_ -> modifyMVar_ server $ \oldServer -> do
        killThread $ serverThread oldServer
        startServer wk

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
