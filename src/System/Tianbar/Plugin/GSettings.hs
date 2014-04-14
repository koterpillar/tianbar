module System.Tianbar.Plugin.GSettings where

-- GSettings plugin

import Control.Monad.IO.Class

import Happstack.Server

import System.Process

import System.Tianbar.Plugin

data GSettings = GSettings

instance Plugin GSettings where
    initialize _ = return GSettings

    handler _ = dir "gsettings" $
        path $ \schema ->
        path $ \key -> do
            nullDir
            setting <- liftIO $ gsettingsGet schema key
            return $ toResponse setting

gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) output

