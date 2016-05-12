-- | This is a very basic system tray widget.  That said, it works
-- very well since it is based on eggtraymanager.
module System.Tianbar.Systray ( systrayNew ) where

import GI.Gtk

import System.Tianbar.Configuration

systrayNew :: IO Widget
systrayNew = do
  undefined
  -- box <- hBoxNew False 5

  -- trayManager <- trayManagerNew
  -- Just screen <- screenGetDefault
  -- _ <- trayManagerManageScreen trayManager screen

  -- _ <- on trayManager trayIconAdded $ \w -> do
  --   widgetShowAll w
  --   boxPackStart box w PackNatural 0

  -- widgetSetSizeRequest box (-1) barHeight

  -- widgetShowAll box
  -- return (toWidget box)
