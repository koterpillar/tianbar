-- | This is a very basic system tray widget.  That said, it works
-- very well since it is based on eggtraymanager.
module System.Tianbar.Systray ( systrayNew ) where

import GI.Gdk.Objects.Display

import GI.Gtk

import Graphics.UI.Gtk.Misc.TrayManager

import System.Tianbar.Configuration

systrayNew :: IO Widget
systrayNew = do
    box <- boxNew OrientationHorizontal 5

    trayManager <- trayManagerNew
    Just disp <- displayGetDefault
    screen <- displayGetScreen disp (fromIntegral myScreen)
    _ <- trayManagerManageScreen trayManager screen

    _ <- onTrayIconAdded trayManager $ \w -> do
        widgetShowAll w
        boxPackStart box w False False 0

    widgetSetSizeRequest box (-1) (fromIntegral barHeight)

    widgetShowAll box
    toWidget box
