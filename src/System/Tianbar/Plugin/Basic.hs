module System.Tianbar.Plugin.Basic where

-- Simple plugins

import Data.List.Split

import Network.URI

import System.Process

import System.Tianbar.Plugin

import Paths_tianbar

-- GSettings plugin
data GSettings = GSettings

instance Plugin GSettings where
    initialize _ = return GSettings
    handleRequest _ = withScheme "gsettings:" $ \uri -> do
        let [schema, key] = splitOn "/" $ uriPath uri
        setting <- gsettingsGet schema key
        return $ Just $ plainContent setting

gsettingsGet :: String -> String -> IO String
gsettingsGet schema key = do
    output <- readProcess "gsettings" ["get", schema, key] []
    let len = length output
    return $ drop 1 $ take (len - 2) output

-- Data directory override
data DataDirectory = DataDirectory

instance Plugin DataDirectory where
    initialize _ = return DataDirectory
    handleRequest _ = withScheme "tianbar:" $ \uri -> do
        let filePath = uriPath uri
        dataFile <- getDataFileName filePath
        return $ Just $ "file://" ++ dataFile

