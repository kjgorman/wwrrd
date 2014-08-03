{-#LANGUAGE ImplicitParams#-}
module WWRRD (
       -- * The phrase set type
         PhraseSet (..)
       -- * functions for interacting with phrase sets
       , loadPhraseSets
       , collectRelations
       , findRelatedPhrases
       , most
       -- * functions for interacting with a WordNet environment
       , closeEnv
       , wnEnv
       ) where

import           Control.Applicative ((<$>))
import           Control.DeepSeq
import           Control.Monad (liftM)
import           Control.Monad.Utilities
import           Control.Parallel.Strategies
import           Data.Char (toLower)
import           Data.Function (on)
import           Data.List (maximumBy)
import           Data.Ord (compare)
import qualified Data.Set as S
import           NLP.WordNet
import           PhraseSet
import           Track

wnEnv :: IO WordNetEnv
wnEnv = initializeWordNetWithOptions (Just dictPath) (return warn)
        where dictPath = "dict/"
              warn s e = return () :: IO ()

closeEnv :: WordNetEnv -> IO ()
closeEnv = closeWordNet

similar :: WN (Word -> POS -> [SearchResult])
similar term pos = search (toLower <$> term) pos AllSenses >>= flip concatMap searchForms . flip relatedBy

searchForms :: [Form]
searchForms = [Similar,Hypernym,Hyponym,Entailment,IsMember,IsStuff,IsPart,HasMember,HasStuff
              ,HasPart,Meronym,Holonym,CauseTo,Derivation,Relatives]

collectSimilar :: WN (String -> [Word])
collectSimilar str = forPos >>= similarWords
    where
      forPos = [(w, p) | p <- [Adj, Noun, Verb, Adv], w <- words str]
      similarWords s = uncurry similar s >>= (`srWords` AllSenses)

pairLyrics :: WN (Track -> PhraseSet)
pairLyrics t = PhraseSet t $ buildPhraseSet <$> lyrics t
    where buildPhraseSet x = PhraseLine x (S.fromList $ collectSimilar x)

collectPhrases :: WN ([Track] -> [PhraseSet])
collectPhrases trax = pairLyrics <$> trax `using` parList rseq

intersectingPhrases :: S.Set Word -> PhraseSet -> PhraseSet
intersectingPhrases rel (PhraseSet t phraseLines) = PhraseSet t $ notEmpty . with $ phraseLines
    where with = map (\(PhraseLine l p) -> PhraseLine l $ S.intersection rel p)
          notEmpty = filter (not . S.null . phrases)

collectIntersecting :: S.Set Word -> [PhraseSet] -> [PhraseSet]
collectIntersecting rel p = filter (not . null . phraseLines) intersecting
  where intersecting = intersectingPhrases rel <$> p `using` rseq

loadPhraseSets :: IO ([PhraseSet], WordNetEnv)
loadPhraseSets = do
  env <- wnEnv
  trax <- parseDirectory "ross"
  let phrases = runs env $ collectPhrases trax
  return (phrases, env)

collectRelations :: WordNetEnv -> [PhraseSet] -> String -> [PhraseSet]
collectRelations env phrases text = collectIntersecting related phrases
  where related = S.fromList $ words text ++ runs env collectSimilar text

most :: [PhraseSet] -> IO PhraseLine
most = return . maximumBy (compare `on` (S.size . phrases)) . concatMap phraseLines

findRelatedPhrases :: [String] -> [PhraseSet] -> IO [PhraseSet]
findRelatedPhrases text phraseSet = do
  env <- wnEnv
  let related = text >>= collectRelations env phraseSet
  related `deepseq` closeEnv env
  return phraseSet
