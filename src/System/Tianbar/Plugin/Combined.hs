{-# LANGUAGE RankNTypes #-}
module System.Tianbar.Plugin.Combined where

import Control.Lens

import Control.Monad

import System.Tianbar.Plugin

data Combined p q = Combined p q

combined1 :: Lens' (Combined p q) p
combined1 inj (Combined p q) = flip Combined q <$> inj p

combined2 :: Lens' (Combined p q) q
combined2 inj (Combined p q) = Combined p <$> inj q

instance (Plugin p, Plugin q) => Plugin (Combined p q) where
    initialize = Combined <$> initialize <*> initialize
    destroy (Combined p q) = destroy p >> destroy q
    handler = runWithLens combined1 handler `mplus` runWithLens combined2 handler

data EmptyPlugin = EmptyPlugin

instance Plugin EmptyPlugin where
    initialize = return EmptyPlugin
    handler = mzero
