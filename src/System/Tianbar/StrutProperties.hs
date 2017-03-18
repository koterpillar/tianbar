{-# LANGUAGE OverloadedStrings #-}
module System.Tianbar.StrutProperties ( setStrutProperties
                                       , StrutProperties ) where

import Control.Monad
import Control.Monad.IO.Class

import Data.GI.Base.ShortPrelude

import qualified GI.Gtk as Gtk

import GI.Gdk.Enums
import GI.Gdk.Objects.Window
import GI.Gdk.Structs.Atom

import Foreign

type StrutProperties = (Int, Int, Int, Int)

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
    (MonadIO m, Storable d) =>
    Window                                  -- window
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
setStrutProperties window (left, right, top, bottom) = do
    let data_ :: [CULong]
        data_ = map fromIntegral [ left
                                 , right
                                 , top
                                 , bottom
                                 ]
    prop <- atomIntern "_NET_WM_STRUT" False
    type_ <- atomIntern "CARDINAL" False
    Just gdkWindow <- Gtk.widgetGetWindow window
    propertyChange gdkWindow prop type_ 32 PropModeReplace data_
