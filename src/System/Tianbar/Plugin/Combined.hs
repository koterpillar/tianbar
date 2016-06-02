{-# LANGUAGE RankNTypes #-}
module System.Tianbar.Plugin.Combined where

import Control.Lens

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe

import System.Tianbar.Plugin

data Combined p q = Combined p q

combined1 :: Lens' (Combined p q) p
combined1 inj (Combined p q) = flip Combined q <$> inj p

combined2 :: Lens' (Combined p q) q
combined2 inj (Combined p q) = Combined p <$> inj q

runWithPart :: Lens' p p' -> ServerPart p' Response -> ServerPart p Response
runWithPart l h' = do
    uri <- ask
    p <- get
    let part = view l p
    (result, part') <- runHandler h' part uri
    put $ set l part' p
    MaybeT $ return result

instance (Plugin p, Plugin q) => Plugin (Combined p q) where
    initialize wk = Combined <$> initialize wk <*> initialize wk
    destroy (Combined p q) = destroy p >> destroy q
    handler = runWithPart combined1 handler `mplus` runWithPart combined2 handler

data EmptyPlugin = EmptyPlugin

instance Plugin EmptyPlugin where
    initialize _ = return EmptyPlugin
    handler = mzero
