{-# LANGUAGE OverloadedStrings #-}
module System.Tianbar.StrutProperties ( setStrutProperties
                                       , StrutProperties ) where

import Control.Monad
import Control.Monad.IO.Class

import Data.GI.Base.ShortPrelude

import qualified GI.Gtk as Gtk

import GI.Gdk.Types
import GI.Gdk.Structs.Atom

import Foreign
import Foreign.C.Types

type StrutProperties = (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)

foreign import ccall "gdk_property_change" gdk_property_change ::
    Ptr Window ->                           -- window : TInterface "Gdk" "Window"
    Ptr Atom ->                             -- property : TInterface "Gdk" "Atom"
    Ptr Atom ->                             -- type : TInterface "Gdk" "Atom"
    Int32 ->                                -- format
    CULong ->                               -- mode : GdkPropMode
    Ptr () ->                               -- data
    Int32 ->                                -- nelements
    IO ()


propertyChange ::
    (MonadIO m, WindowK a, Storable d) =>
    a                                       -- window
    -> Atom                                 -- property
    -> Atom                                 -- type_
    -> Int32                                -- format
    -> PropMode                             -- mode
    -> [d]                                  -- elements
    -> m ()
propertyChange window property type_ format mode elements = liftIO $ do
    let window' = unsafeManagedPtrCastPtr window
    let property' = unsafeManagedPtrGetPtr property
    let type_' = unsafeManagedPtrGetPtr type_
    let nelements = length elements
    let mode' = fromIntegral $ fromEnum mode
    data_ <- allocBytes (sizeOf (head elements) * nelements) :: IO (Ptr d)
    forM_ (zip [0..] elements) $ uncurry $ pokeElemOff data_
    gdk_property_change window' property' type_' format mode' (castPtr data_) (fromIntegral nelements)
    touchManagedPtr window
    touchManagedPtr property
    touchManagedPtr type_
    freeMem data_


-- | Reserve EWMH struts
setStrutProperties :: Gtk.Window -> StrutProperties -> IO ()
setStrutProperties window (left, right, top, bottom,
                                left_start_y, left_end_y,
                                right_start_y, right_end_y,
                                top_start_x, top_end_x,
                                bottom_start_x, bottom_end_x) = do
    let data_ :: [CULong]
        data_ = map fromIntegral [ left
                                 , right
                                 , top
                                 , bottom
                                 , left_start_y
                                 , left_end_y
                                 , right_start_y
                                 , right_end_y
                                 , top_start_x
                                 , top_end_x
                                 , bottom_start_x
                                 , bottom_end_x
                                 ]
    prop <- atomIntern "_NET_WM_STRUT_PARTIAL" False
    type_ <- atomIntern "CARDINAL" False
    Just gdkWindow <- Gtk.widgetGetWindow window
    propertyChange gdkWindow prop type_ 32 PropModeReplace data_
