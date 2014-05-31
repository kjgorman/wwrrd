{-#LANGUAGE ImplicitParams#-}
module WWRRD (
       -- * The phrase set type
         PhraseSet (..)
       -- * functions for interacting with phrase sets
       , loadPhraseSets
       , collectRelations
       -- * functions for interacting with a WordNet environment
       , closeEnv
       , wnEnv
       ) where

import           Control.Applicative ((<$>))
import           Control.Monad (join)
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

closeEnv :: WordNetEnv -> IO ()
closeEnv = closeWordNet

similar :: WN (Word -> POS -> [SearchResult])
similar term pos = search (map toLower term) pos AllSenses >>= relatedBy Similar

collectSimilar :: WN (String -> [Word])
collectSimilar str = forPos >>= similarWords
    where
      forPos = [(w, p) | p <- [Adj, Noun, Verb, Adv], w <- words str]
      similarWords s = uncurry similar s >>= (`srWords` AllSenses)

data PhraseSet = PhraseSet { track :: Track
                           , line :: Word
                           , phrases :: S.Set Word }
                 deriving (Show)

pairLyrics :: WN (Track -> [PhraseSet])
pairLyrics t = map buildPhraseSet $ lyrics t
    where buildPhraseSet x = PhraseSet t x (S.fromList $ collectSimilar x)

collectPhrases :: WN ([Track] -> [PhraseSet])
collectPhrases trax = join (pairLyrics <$> trax `using` parList rseq)

intersectingPhrases :: S.Set Word -> PhraseSet -> PhraseSet
intersectingPhrases rel p = PhraseSet (track p) (line p) (S.intersection rel $ phrases p)

collectIntersecting :: S.Set Word -> [PhraseSet] -> [PhraseSet]
collectIntersecting rel p = intersectingPhrases rel <$> p `using` rseq

loadPhraseSets :: IO ([PhraseSet], WordNetEnv)
loadPhraseSets = do
  env <- wnEnv
  trax <- parseDirectory "ross"
  let phrases = runs env $ collectPhrases trax
  return (phrases, env)

collectRelations :: WordNetEnv -> String -> [PhraseSet] -> [PhraseSet]
collectRelations env text = collectIntersecting related
  where related = S.fromList $ words text ++ runs env collectSimilar text
