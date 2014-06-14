module Pick (
         pick
       ) where

import           Control.Monad (liftM)
import           System.Random (randomRIO)

pick :: [a] -> IO (Maybe a)
pick [] = return Nothing
pick xs = liftM (Just . (xs !!)) $ randomRIO (lo, hi)
          where lo = 0
                hi = length xs - 1
