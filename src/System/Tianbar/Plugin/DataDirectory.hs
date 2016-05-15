module System.Tianbar.Plugin.DataDirectory where

-- Serve files from the data directory

import Control.Monad.Trans
import Control.Monad.Trans.Reader

import Network.URI

import System.Tianbar.Plugin

import Paths_tianbar

data DataDirectory = DataDirectory

instance Plugin DataDirectory where
    initialize _ = return DataDirectory

    handler _ = dir "data" $ do
        uri <- lift $ ask
        let filePath = uriPath uri
        dataFile <- liftIO $ getDataFileName filePath
        serveFile dataFile
