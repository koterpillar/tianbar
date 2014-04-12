module System.Tianbar where

import Graphics.UI.Gtk hiding (Signal)

import System.Tianbar.Configuration
import System.Tianbar.Server
import System.Tianbar.Systray
import System.Tianbar.StrutProperties
import System.Tianbar.WebKit

topStrut :: Rectangle -> StrutProperties
topStrut (Rectangle mX mY mW _) = (0, 0, h, 0, 0, 0, 0, 0, x, x + w, 0, 0)
    where x = mX
          w = mW - 1
          h = barHeight + mY

main :: IO ()
main = do
    serverPort <- startServer

    _ <- initGUI

    Just disp <- displayGetDefault
    screen <- displayGetScreen disp myScreen
    monitorSize <- screenGetMonitorGeometry screen myMonitor

    window <- windowNew
    widgetSetName window appName

    let Rectangle x _ w _ = monitorSize
    windowSetTypeHint window WindowTypeHintDock
    windowSetScreen window screen
    windowSetDefaultSize window w barHeight
    windowMove window x 0
    _ <- onRealize window $
        setStrutProperties window $ topStrut monitorSize

    box <- hBoxNew False widgetSpacing
    containerAdd window box

    wk <- tianbarWebkitNew serverPort
    boxPackStart box wk PackGrow 0

    tray <- systrayNew
    boxPackEnd box tray PackNatural 0

    widgetShow window
    widgetShow box

    mainGUI
    return ()
