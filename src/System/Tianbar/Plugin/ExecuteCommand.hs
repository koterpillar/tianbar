module System.Tianbar.Plugin.ExecuteCommand where

-- Execute arbitrary commands

import Control.Monad.Trans

import System.Process

import System.Tianbar.Plugin

data ExecuteCommand = ExecuteCommand

instance Plugin ExecuteCommand where
    initialize _ = return ExecuteCommand

    handler = dir "execute" executeHandler

executeHandler :: Handler ExecuteCommand Response
executeHandler = do
    nullDir
    command <- look "command"
    output <- liftIO $ readCreateProcess (shell command) ""
    stringResponse output
