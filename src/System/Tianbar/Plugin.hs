module System.Tianbar.Plugin (
    Plugin (..),
) where

import Happstack.Server

import System.Tianbar.Callbacks

class Plugin p where
    initialize :: Callbacks -> IO p

    destroy :: p -> IO ()
    destroy _ = return ()

    handler :: p -> ServerPartT IO Response
