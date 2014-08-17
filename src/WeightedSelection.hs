module WeightedSelection (
    select
  ) where

import Control.Arrow ((&&&))
import Control.Monad.Random

select :: (a -> Rational) -> [a] -> IO a
select ranking = evalRandIO . fromList . map (id &&& ranking)
