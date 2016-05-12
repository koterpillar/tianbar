module System.Tianbar.Plugin.Combined where

import Control.Monad

import System.Tianbar.Plugin

data Combined p q = Combined p q

instance (Plugin p, Plugin q) => Plugin (Combined p q) where
    initialize wk = Combined <$> initialize wk <*> initialize wk
    destroy (Combined p q) = destroy p >> destroy q
    handler (Combined p q) = handler p `mplus` handler q

data Empty = Empty

instance Plugin Empty where
    initialize _ = return Empty
    handler _ = mzero
