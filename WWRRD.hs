{-#LANGUAGE ImplicitParams#-}
module WWRRD where

import           Control.Applicative ((<$>))
import           Control.Parallel.Strategies
import           Data.Char (toLower)
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
similar term pos = search (map toLower term) pos AllSenses >>= relatedBy Similar

collectSimilar :: WN (String -> [Word])
collectSimilar str = forPos >>= similarWords
    where
      forPos = [(w, p) | p <- [Adj, Noun, Verb, Adv], w <- words str]
      similarWords s = uncurry similar s >>= (`srWords` AllSenses)

type PhraseSet = (String, S.Set Word)

pairLyrics :: WN (Track -> [PhraseSet])
pairLyrics = map (\x -> (x, S.fromList $ collectSimilar x)) . lyrics

collectPhrases :: WN ([Track] -> [[PhraseSet]])
collectPhrases trax = pairLyrics <$> trax `using` parList rseq

intersectingPhrases :: S.Set Word -> [PhraseSet] -> [PhraseSet]
intersectingPhrases rel = filter (not . S.null . S.intersection rel . snd)

collectIntersecting :: S.Set Word -> [[PhraseSet]] -> [[PhraseSet]]
collectIntersecting rel p = intersectingPhrases rel <$> p `using` rseq

main :: IO ()
main = do
  env <- wnEnv
  trax <- parseDirectory "ross"
  let phrases = runs env $ collectPhrases trax
  loop phrases env
  closeWordNet env
  where
    loop phrases env = do
      phrase <- readLn
      case phrase of
        "" -> return ()
        text -> getIntersection phrases env text
    getIntersection phrases env text = do
      let related = S.fromList $ words text ++ runs env collectSimilar text
          inter   = collectIntersecting related phrases
      print $ map fst <$> inter
      loop phrases env
