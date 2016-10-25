module System.Tianbar.Plugin.ExecuteCommand where

-- Execute arbitrary commands

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans

import System.Process

import System.Tianbar.Plugin
import System.Tianbar.RequestResponse

data ExecuteCommand = ExecuteCommand

instance Plugin ExecuteCommand where
    initialize = return ExecuteCommand

    handler = msum [ dir "execute" executeHandler
                   , dir "spawn" launchHandler
                   ]

executeHandler :: ServerPart ExecuteCommand Response
executeHandler = do
    nullDir
    command <- look "command"
    output <- liftIO $ readCreateProcess (shell command) ""
    stringResponse output


launchHandler :: ServerPart ExecuteCommand Response
launchHandler = do
    nullDir
    command <- look "command"
    _ <- liftIO $ do
        process <- spawnCommand command
        forkIO $ do
            _ <- waitForProcess process
            return ()
    okResponse
