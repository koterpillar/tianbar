module System.Tianbar.Plugin.ExecuteCommand where

-- Execute arbitrary commands

import Control.Monad.Trans

import System.Process

import System.Tianbar.Plugin
import System.Tianbar.RequestResponse

data ExecuteCommand = ExecuteCommand

instance Plugin ExecuteCommand where
    initialize = return ExecuteCommand

    handler = dir "execute" executeHandler

executeHandler :: ServerPart ExecuteCommand Response
executeHandler = do
    nullDir
    command <- look "command"
    output <- liftIO $ readCreateProcess (shell command) ""
    stringResponse output
