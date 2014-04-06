module System.Tianbar.Plugin.Combined where

import Control.Monad

import System.Tianbar.Plugin

data Combined p q = Combined p q

instance (Plugin p, Plugin q) => Plugin (Combined p q) where
    initialize wk = liftM2 Combined (initialize wk) (initialize wk)

    destroy (Combined p q) = destroy p >> destroy q

    handleRequest (Combined p q) s = mplus rp rq
        where rp = handleRequest p s
              rq = handleRequest q s

mergeOverrides :: [UriHandler] -> UriHandler
mergeOverrides overrides = foldr mplus Nothing . flip map overrides . flip ($)

data Empty = Empty

instance Plugin Empty where
    simpleInitialize = Empty
