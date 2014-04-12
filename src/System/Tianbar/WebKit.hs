{-# Language OverloadedStrings #-}
module System.Tianbar.WebKit where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import Graphics.UI.Gtk hiding (disconnect, Signal, Variant)
import Graphics.UI.Gtk.WebKit.NetworkRequest
import Graphics.UI.Gtk.WebKit.WebSettings
import Graphics.UI.Gtk.WebKit.WebView
import Graphics.UI.Gtk.WebKit.WebWindowFeatures

import Network.Socket
import Network.URI

import System.Environment.XDG.BaseDir

import System.Tianbar.Configuration
import System.Tianbar.Utils

tianbarWebView :: PortNumber -> IO WebView
tianbarWebView portNum = do
    wk <- webViewNew

    -- Enable AJAX access to all domains
    wsettings <- webViewGetWebSettings wk
    set wsettings [webSettingsEnableUniversalAccessFromFileUris := True]
    webViewSetWebSettings wk wsettings

    -- Process the special overrides
    _ <- on wk resourceRequestStarting $ \_ _ nreq _ -> void $ runMaybeT $ do
        req <- liftMT nreq
        uriStr <- MaybeT $ networkRequestGetUri req
        uri <- liftMT $ parseURI uriStr
        when (uriScheme uri == "tianbar:") $ do
            -- TODO: append something secret to the URI
            let uri' = uri { uriScheme = "http:"
                           , uriAuthority = Just URIAuth { uriUserInfo = ""
                                                         , uriRegName = "localhost"
                                                         , uriPort = ':' : show portNum
                                                         }
                           }
            liftIO $ networkRequestSetUri req $ show uri'

    -- Handle new window creation
    _ <- on wk createWebView $ \_ -> do
        nwk <- tianbarWebView portNum

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

tianbarWebkitNew :: PortNumber -> IO Widget
tianbarWebkitNew portNum = do
    l <- tianbarWebView portNum

    _ <- on l realize $ loadIndexPage l

    Just disp <- displayGetDefault
    screen <- displayGetScreen disp myScreen
    (Rectangle _ _ sw _) <- screenGetMonitorGeometry screen myMonitor
    _ <- on l sizeRequest $ return (Requisition (sw `div` 2) barHeight)

    widgetShowAll l
    return (toWidget l)
