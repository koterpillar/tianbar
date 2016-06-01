module System.Tianbar.Plugin.FileSystem where

-- Serve files from the data directory

import Control.Monad
import Control.Monad.Reader

import qualified Data.List as L
import qualified Data.Text as T

import System.Environment.XDG.BaseDir

import System.Tianbar.Plugin
import System.Tianbar.Configuration

import Paths_tianbar

data FileSystem = FileSystem

getUserFileName :: String -> IO FilePath
getUserFileName = getUserConfigFile appName

getRootFileName :: String -> IO FilePath
getRootFileName filePath = return $ "/" ++ filePath

instance Plugin FileSystem where
    initialize _ = return FileSystem

    handler = msum [ dir "data" $ directoryHandler getDataFileName
                   , dir "user" $ directoryHandler getUserFileName
                   , dir "root" $ directoryHandler getRootFileName
                   ]

directoryHandler :: (String -> IO FilePath) -> Handler FileSystem Response
directoryHandler getFileName = do
    uri <- ask
    let filePath = L.intercalate "/" $ map T.unpack $ uriPathSegments uri
    dataFile <- liftIO $ getFileName filePath
    serveFile dataFile
