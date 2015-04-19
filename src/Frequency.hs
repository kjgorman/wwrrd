module Frequency (
                  build,
                  score
                  ) where

import           Data.Foldable (foldl')
import qualified Data.Map.Strict as M
import           PhraseSet (PhraseSet, track)
import           Track (lyrics)
import           RedisClient

build :: IO (M.Map String Int)
build = do
  phrases <- readPhrasesFromStore
  return $ count phrases

count :: [PhraseSet] -> M.Map String Int
count = foldl' countTrack M.empty
    where
      trackWords p = concatMap words $ lyrics (track p)
      countTrack m p = foldl' (\m w -> M.insertWith (+) w 1 m) m $ trackWords p

mmax :: Ord b => M.Map a b -> b
mmax = foldl1 (\l v -> if v > l then v else l) . M.elems

score :: Floating a => M.Map String Int -> String -> a
score counts word = let max  = mmax counts
                        this = M.findWithDefault 0 word counts
                    in 1 - (fromIntegral this / fromIntegral max)
