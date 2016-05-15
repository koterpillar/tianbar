module System.Tianbar.Plugin.DataDirectory where

-- Serve files from the data directory

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Reader

import qualified Data.List as L
import qualified Data.Text as T

import System.Environment.XDG.BaseDir

import System.Tianbar.Plugin
import System.Tianbar.Configuration

import Paths_tianbar

data DataDirectory = DataDirectory

getUserFileName :: String -> IO FilePath
getUserFileName = getUserConfigFile appName

instance Plugin DataDirectory where
    initialize _ = return DataDirectory

    handler _ = msum [ dir "data" $ directoryHandler getDataFileName
                     , dir "user" $ directoryHandler getUserFileName
                     ]

directoryHandler :: (String -> IO FilePath) -> Handler Response
directoryHandler getFileName = do
    uri <- lift $ ask
    let filePath = L.intercalate "/" $ map T.unpack $ uriPathSegments uri
    dataFile <- liftIO $ getFileName filePath
    serveFile dataFile
