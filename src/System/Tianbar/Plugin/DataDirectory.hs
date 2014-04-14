module System.Tianbar.Plugin.DataDirectory where

-- Serve files from the data directory

import Control.Monad.IO.Class

import Happstack.Server

import System.Tianbar.Plugin

import Paths_tianbar

data DataDirectory = DataDirectory

instance Plugin DataDirectory where
    initialize _ = return DataDirectory

    handler _ = dir "data" $ uriRest $ \filePath -> do
        dataFile <- liftIO $ getDataFileName filePath
        serveFile (guessContentTypeM mimeTypes) dataFile
