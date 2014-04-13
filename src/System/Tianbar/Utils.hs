module System.Tianbar.Utils where

import Control.Monad.Trans.Maybe

liftMT :: Monad m => Maybe a -> MaybeT m a
liftMT = MaybeT . return
