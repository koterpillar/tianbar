{-# Language OverloadedStrings #-}
module System.Tianbar.WebKit where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

import qualified Data.Text as T
import Data.GI.Base

import GI.Gdk.Flags
import GI.Gdk.Objects.Display
import GI.Gdk.Objects.Screen
import GI.Gdk.Structs.Geometry
import GI.Gdk.Structs.Rectangle

import qualified GI.GLib as GLib

import GI.Gtk hiding (main)
import GI.Gtk.Objects.Widget

import GI.WebKit2.Interfaces.PermissionRequest (permissionRequestAllow)
import GI.WebKit2.Objects.Settings
import GI.WebKit2.Objects.WebResource
import GI.WebKit2.Objects.WebView
import GI.WebKit2.Objects.WindowProperties

import GI.Extras
import GI.Signals

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
    wsettings <- webViewGetSettings wk
    settingsSetAllowFileAccessFromFileUrls wsettings True
    webViewSetSettings wk wsettings

    -- Enable geolocation
    _ <- on wk PermissionRequest $ \request -> do
        -- TODO: This should only match geolocation permission requests
        permissionRequestAllow request
        return True

    -- Initialize plugins, and re-initialize on reloads
    server <- startServer (callbacks wk) >>= newMVar
    _ <- on wk LoadChanged $ \_ -> modifyMVar_ server $ \oldServer -> do
        stopServer oldServer
        startServer (callbacks wk)

    -- Process the special overrides
    -- FIXME: enable
    -- _ <- on wk ResourceLoadStarted $ \_ _ nreq _ -> void $ runMaybeT $ do
    --     req <- liftMT nreq
    --     uriStr <- MaybeT $ webResourceGetUri req
    --     uri <- liftMT $ parseURI uriStr
    --     override <- liftIO $ withMVar server $ return . serverOverrideURI
    --     let uri' = override uri
    --     liftIO $ networkRequestSetUri req $ show uri'

    -- Handle new window creation
    _ <- on wk Create $ \_ -> do
        nwk <- tianbarWebView

        window <- windowNew WindowTypePopup
        containerAdd window nwk

        _ <- on nwk ReadyToShow $ do
            wprop <- webViewGetWindowProperties nwk
            wgeom <- getWindowPropertiesGeometry wprop

            [wx, wy, ww, wh] <- mapM ($ wgeom) [ rectangleReadX
                                               , rectangleReadY
                                               , rectangleReadWidth
                                               , rectangleReadHeight
                                               ]

            geomHint <- newZeroGeometry
            geometryWriteMinWidth geomHint ww
            geometryWriteMinHeight geomHint wh
            geometryWriteMaxWidth geomHint ww
            geometryWriteMaxHeight geomHint wh
            windowSetGeometryHints window
                (Nothing :: Maybe Widget)
                (Just geomHint)
                [WindowHintsMinSize, WindowHintsMaxSize]

            widgetShow window
            widgetShow nwk

            windowMove window wx wy
            windowSetKeepAbove window True
            windowStick window

            return ()

        toWidget nwk

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

    webViewLoadUri wk $ T.pack $ "file://" ++ htmlFile

tianbarWebkitNew :: IO Widget
tianbarWebkitNew = do
    l <- tianbarWebView

    _ <- on l Realize $ loadIndexPage l

    disp <- displayGetDefault
    screen <- displayGetScreen disp (fromIntegral myScreen)
    monitorSize <- screenGetMonitorGeometry screen (fromIntegral myMonitor)
    monitorW <- rectangleReadWidth monitorSize

    widgetSetSizeRequest l (monitorW `div` 2) (fromIntegral barHeight)

    widgetShowAll l
    toWidget l
