{-# Language OverloadedStrings #-}
module System.Tianbar.WebKit where

import Control.Concurrent
import Control.Exception
import Control.Monad

import qualified Data.Text as T
import Data.GI.Base

import GI.Gdk.Flags
import GI.Gdk.Objects.Display
import GI.Gdk.Objects.Screen
import GI.Gdk.Structs.Geometry
import GI.Gdk.Structs.Rectangle

import GI.Gio.Objects.MemoryInputStream

import GI.GLib.Callbacks
import GI.GLib.Functions hiding (getUserConfigDir)

import GI.Gtk hiding (main)

import GI.WebKit2.Callbacks
import GI.WebKit2.Enums
import GI.WebKit2.Interfaces.PermissionRequest (permissionRequestAllow)
import GI.WebKit2.Objects.SecurityManager
import GI.WebKit2.Objects.Settings
import GI.WebKit2.Objects.URISchemeRequest
import GI.WebKit2.Objects.WebContext
import GI.WebKit2.Objects.WebView
import GI.WebKit2.Objects.WindowProperties

import System.Directory
import System.Environment.XDG.BaseDir

import System.Tianbar.Plugin
import System.Tianbar.Configuration
import System.Tianbar.Server

import Paths_tianbar


tianbarWebView :: IO WebView
tianbarWebView = do
    wk <- webViewNew

    -- Debugging settings
    wsettings <- webViewGetSettings wk
    settingsSetEnableWriteConsoleMessagesToStdout wsettings True
    settingsSetEnableDeveloperExtras wsettings True
    webViewSetSettings wk wsettings

    -- Enable geolocation
    _ <- onWebViewPermissionRequest wk $ \request -> do
        -- TODO: This should only match geolocation permission requests
        permissionRequestAllow request
        return True

    -- Initialize plugins, and re-initialize on reloads
    server <- startServer wk >>= newMVar
    -- TODO: Destroy server (stopServer) on destroying the WebView

    -- All Tianbar plugins are served under tianbar://, allow it for CORS and
    -- register its handler
    ctx <- webViewGetContext wk
    sec <- webContextGetSecurityManager ctx
    securityManagerRegisterUriSchemeAsCorsEnabled sec "tianbar"
    webContextRegisterUriScheme ctx "tianbar" (handleRequest server)

    -- Handle new window creation
    _ <- onWebViewCreate wk $ \_ -> do
        nwk <- tianbarWebView

        window <- windowNew WindowTypePopup
        containerAdd window nwk

        _ <- onWebViewReadyToShow nwk $ do
            wprop <- webViewGetWindowProperties nwk
            Just wgeom <- getWindowPropertiesGeometry wprop

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
                noWidget
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


handleRequest :: MVar Server -> URISchemeRequestCallback
handleRequest server ureq = do
    uriStr <- uRISchemeRequestGetUri ureq
    let uri = parseURI uriStr
    response <- catch
        (fmap Right $ withMVar server $ \srv -> handleURI srv uri)
        (\e -> return $ Left (e :: SomeException))
    case response of
      Left exc -> do
          putStrLn $ "Error on URI: " ++ T.unpack uriStr
          err <- tianbarError 500 (show exc)
          uRISchemeRequestFinishError ureq err
      Right Nothing -> do
          putStrLn $ "URI invalid: " ++ T.unpack uriStr
          err <- tianbarError 404 "Invalid tianbar: URI"
          uRISchemeRequestFinishError ureq err
      Right (Just resp') -> do
          stream <- memoryInputStreamNewFromData (content resp') noDestroyNotify
          uRISchemeRequestFinish ureq stream (-1) (T.pack <$> mimeType resp')


tianbarError :: Int -> String -> IO GError
tianbarError code message = do
    errDomain <- quarkFromString (Just $ T.pack "Tianbar")
    gerrorNew errDomain (fromIntegral code) (T.pack message)


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

    webViewLoadUri wk $ T.pack "tianbar:///user/index.html"


tianbarWebkitNew :: IO Widget
tianbarWebkitNew = do
    wv <- tianbarWebView

    _ <- onWidgetRealize wv $ loadIndexPage wv

    Just disp <- displayGetDefault
    screen <- displayGetDefaultScreen disp
    monitorSize <- screenGetMonitorGeometry screen (fromIntegral myMonitor)
    monitorW <- rectangleReadWidth monitorSize

    widgetSetSizeRequest wv (monitorW `div` 2) (fromIntegral barHeight)

    widgetShowAll wv
    toWidget wv
