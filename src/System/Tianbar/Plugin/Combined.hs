module System.Tianbar.Plugin.Combined where

import Control.Monad

import System.Tianbar.Plugin

data Combined p q = Combined p q

instance (Plugin p, Plugin q) => Plugin (Combined p q) where
    initialize wk = liftM2 Combined (initialize wk) (initialize wk)

    destroy (Combined p q) = destroy p >> destroy q

    handleRequest (Combined p q) s = do
        rp <- handleRequest p s
        case rp of
            Just _ -> return rp
            Nothing -> handleRequest q s

data Empty = Empty

instance Plugin Empty where
    initialize _ = return Empty
