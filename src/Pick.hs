module Pick (
         pick
       ) where

import           Control.Monad (liftM)
import           System.Random (randomRIO)

-- for some brain dead reason choice in random.extras is a partial fn

pick :: [a] -> IO (Maybe a)
pick [] = return Nothing
pick xs = liftM (Just . (xs !!)) $ randomRIO (lo, hi)
          where lo = 0
                hi = length xs - 1
