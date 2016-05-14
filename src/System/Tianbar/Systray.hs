-- | This is a very basic system tray widget.  That said, it works
-- very well since it is based on eggtraymanager.
module System.Tianbar.Systray ( systrayNew ) where

import GI.Gdk.Objects.Display
import GI.Gdk.Objects.Screen

import GI.Gtk
import GI.Gtk.Enums

import System.Tianbar.Configuration

systrayNew :: IO Widget
systrayNew = do
    box <- boxNew OrientationHorizontal 5

    -- trayManager <- trayManagerNew
    -- disp <- displayGetDefault
    -- screen <- displayGetScreen disp (fromIntegral myScreen)
    -- _ <- undefined trayManagerManageScreen trayManager screen

    -- _ <- on trayManager undefined trayIconAdded $ \w -> do
    --     widgetShowAll w
    --     boxPackStart box w False False 0

    widgetSetSizeRequest box (-1) (fromIntegral barHeight)

    widgetShowAll box
    toWidget box
