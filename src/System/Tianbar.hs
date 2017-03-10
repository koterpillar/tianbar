module System.Tianbar where

import Data.GI.Base
import Data.Int
import qualified Data.Text as T

import GI.Gdk.Enums hiding (WindowTypeToplevel)
import GI.Gdk.Objects.Display
import GI.Gdk.Objects.Monitor
import GI.Gdk.Objects.Screen
import qualified GI.Gdk.Structs.Rectangle as R

import GI.Gtk.Enums
import qualified GI.Gtk.Functions as GtkFunctions
import GI.Gtk.Objects.Box
import GI.Gtk.Objects.Container
import GI.Gtk.Objects.Widget
import GI.Gtk.Objects.Window

import System.Environment (getArgs, getProgName)

import System.Posix.Resource

import System.Tianbar.Configuration
import System.Tianbar.StrutProperties
import System.Tianbar.WebKit


data Rectangle = Rectangle { rX :: Int32
                           , rY :: Int32
                           , rW :: Int32
                           , rH :: Int32
                           }

-- The rectangle Tianbar should occupy on the display
tianbarRectangle :: IO Rectangle
tianbarRectangle = do
    Just disp <- displayGetDefault
    Just monitor <- displayGetPrimaryMonitor disp
    monitorSize <- monitorGetGeometry monitor

    monitorX <- get monitorSize R.rectangleX
    monitorY <- get monitorSize R.rectangleY
    monitorW <- get monitorSize R.rectangleWidth

    return $ Rectangle monitorX monitorY monitorW (fromIntegral barHeight)


topStrut :: StrutProperties
topStrut = (0, 0, barHeight, 0)


sizeMainWindow :: Window -> IO ()
sizeMainWindow window = do
    size <- tianbarRectangle

    windowSetDefaultSize window (rW size) (rH size)
    windowMove window (rX size) (rY size)
    windowResize window (rW size) (rH size)
    setStrutProperties window topStrut


memoryLimit :: ResourceLimits
memoryLimit = ResourceLimits limit limit
  where
    limit = ResourceLimit $ 1024 * 1024 * 800


main :: IO ()
main = do
    setResourceLimit ResourceDataSize memoryLimit

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
