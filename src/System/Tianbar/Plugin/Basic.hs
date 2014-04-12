module System.Tianbar.Plugin.Basic where

-- Simple plugins

import Control.Monad.IO.Class

import Happstack.Server

import Paths_tianbar

dataDirectory :: ServerPartT IO Response
dataDirectory = dir "data" $ uriRest $ \filePath -> do
    dataFile <- liftIO $ getDataFileName filePath
    serveFile (guessContentTypeM mimeTypes) dataFile
