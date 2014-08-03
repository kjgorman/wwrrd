module Control.Monad.Utilities (
    concatMapM
  ) where

import Control.Monad (liftM)

-- how does this not exist in control.monad?
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = liftM concat . mapM f
