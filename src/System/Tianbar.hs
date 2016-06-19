module System.Tianbar where

import qualified Data.Text as T

import GI.Gdk.Enums hiding (WindowTypeToplevel)
import GI.Gdk.Objects.Display
import GI.Gdk.Objects.Screen
import GI.Gdk.Structs.Rectangle

import GI.Gtk.Enums
import qualified GI.Gtk.Functions as GtkFunctions
import GI.Gtk.Objects.Box
import GI.Gtk.Objects.Container
import GI.Gtk.Objects.Widget
import GI.Gtk.Objects.Window

import System.Environment (getArgs, getProgName)

import System.Tianbar.Configuration
import System.Tianbar.StrutProperties
import System.Tianbar.WebKit


topStrut :: Rectangle -> IO StrutProperties
topStrut rect = do
    mX <- rectangleReadX rect
    mY <- rectangleReadY rect
    mW <- rectangleReadWidth rect
    let x = fromIntegral mX
        w = fromIntegral mW - 1
        h = barHeight + fromIntegral mY
     in return (0, 0, h, 0, 0, 0, 0, 0, x, x + w, 0, 0)


sizeMainWindow :: Window -> IO ()
sizeMainWindow window = do
    Just disp <- displayGetDefault
    screen <- displayGetDefaultScreen disp
    monitorSize <- screenGetMonitorGeometry screen (fromIntegral myMonitor)

    monitorX <- rectangleReadX monitorSize
    monitorY <- rectangleReadY monitorSize
    monitorW <- rectangleReadWidth monitorSize

    let windowX = monitorX
    let windowY = monitorY
    let windowWidth = fromIntegral monitorW
    let windowHeight = fromIntegral barHeight

    windowSetDefaultSize window (fromIntegral monitorW) (fromIntegral barHeight)
    windowMove window windowX windowY
    windowResize window windowWidth windowHeight

    strut <- topStrut monitorSize
    setStrutProperties window strut


main :: IO ()
main = do
    progName <- getProgName
    args <- getArgs
    _ <- GtkFunctions.init $ Just $ map T.pack (progName : args)

    Just disp <- displayGetDefault
    screen <- displayGetDefaultScreen disp

    window <- windowNew WindowTypeToplevel
    widgetSetName window $ T.pack appName

    windowSetTypeHint window WindowTypeHintDock
    windowSetScreen window screen

    _ <- onWidgetRealize window $ sizeMainWindow window
    _ <- onScreenMonitorsChanged screen $ sizeMainWindow window

    box <- boxNew OrientationHorizontal 0
    containerAdd window box

    wk <- tianbarWebkitNew
    boxPackStart box wk True True 0

    widgetShow window
    widgetShow box

    GtkFunctions.main
