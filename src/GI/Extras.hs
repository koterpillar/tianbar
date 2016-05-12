module GI.Extras where

import Data.GI.Base.ShortPrelude

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
