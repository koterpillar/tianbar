module System.Tianbar.Plugin.DataDirectory where

-- Serve files from the data directory

import Control.Monad.Trans
import Control.Monad.Trans.Reader

import qualified Data.List as L
import qualified Data.Text as T

import System.Tianbar.Plugin

import Paths_tianbar

data DataDirectory = DataDirectory

instance Plugin DataDirectory where
    initialize _ = return DataDirectory

    handler _ = dir "data" $ do
        uri <- lift $ ask
        let filePath = L.intercalate "/" $ map T.unpack $ uriPathSegments uri
        dataFile <- liftIO $ getDataFileName filePath
        serveFile dataFile
