module GI.Extras where

import Data.GI.Base.ShortPrelude

import GI.Gdk.Structs.Geometry
import GI.Gdk.Structs.Rectangle

rectangleWriteX :: MonadIO m => Rectangle -> Int32 -> m ()
rectangleWriteX s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Int32)

rectangleWriteY :: MonadIO m => Rectangle -> Int32 -> m ()
rectangleWriteY s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 4) (val :: Int32)

rectangleWriteWidth :: MonadIO m => Rectangle -> Int32 -> m ()
rectangleWriteWidth s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Int32)

rectangleWriteHeight :: MonadIO m => Rectangle -> Int32 -> m ()
rectangleWriteHeight s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 12) (val :: Int32)

geometryWriteMinWidth :: MonadIO m => Geometry -> Int32 -> m ()
geometryWriteMinWidth s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 0) (val :: Int32)

geometryWriteMinHeight :: MonadIO m => Geometry -> Int32 -> m ()
geometryWriteMinHeight s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 4) (val :: Int32)

geometryWriteMaxWidth :: MonadIO m => Geometry -> Int32 -> m ()
geometryWriteMaxWidth s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 8) (val :: Int32)

geometryWriteMaxHeight :: MonadIO m => Geometry -> Int32 -> m ()
geometryWriteMaxHeight s val = liftIO $ withManagedPtr s $ \ptr -> do
    poke (ptr `plusPtr` 12) (val :: Int32)
