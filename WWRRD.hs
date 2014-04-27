{-#LANGUAGE ImplicitParams#-}
module WWRRD where

import           Data.Function
import           Data.List (intercalate, sortBy)
import           Data.Ord
import qualified Data.Set as S
import           NLP.WordNet
import           Track

wnEnv :: IO WordNetEnv
wnEnv = initializeWordNetWithOptions (Just dictPath) (return warn)
        where dictPath = "dict/"
              warn s e = return () :: IO ()

similar :: WN (Word -> POS -> [SearchResult])
similar term pos = (search term pos AllSenses) >>= relatedBy Similar

collectSimilar :: WN (String -> [Word])
collectSimilar str = forPos >>= similarWords
    where
      tkns = words str
      forPos = [(w, p) | p <- [Adj, Noun, Verb, Adv], w <- tkns]
      similarWords s = similar (fst s) (snd s) >>= (flip srWords $ AllSenses)

pairLyrics :: WN (Track -> [(String, S.Set Word)])
pairLyrics = map (\x -> (x, S.fromList $ collectSimilar x)) . lyrics

main :: IO ()
main = do
  env <- wnEnv
  trax <- parseDirectory "ross"
  phrase <- readLn
  let related = S.fromList $ words phrase ++ (runs env $ collectSimilar phrase)
      test = runs env $ pairLyrics $ head trax
      inter = filter (not . S.null . (S.intersection related) . snd) test
      ordering = sortBy (compare `on` (S.size . snd))
  putStrLn . show . fst . head . ordering $ inter
