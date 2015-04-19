{-#LANGUAGE ImplicitParams#-}
module WWRRD (
       -- * The phrase set type
         PhraseSet (..)
       -- * functions for interacting with phrase sets
       , loadPhraseSets
       , findRelatedPhrases
       , getRelated
       ) where

import           Control.Applicative ((<$>))
import           Control.Monad (liftM)
import           Control.Parallel.Strategies
import           Data.Char     (toLower)
import qualified Data.Set as S
import           NLP.WordNet
import           PhraseSet
import           Track
import           Relatedness (findRelatedPhrases, run, relatedPhrase)

getRelated :: String -> IO (S.Set String)
getRelated = run relatedPhrase

pairLyrics :: WN (Track -> PhraseSet)
pairLyrics t = PhraseSet t $ buildPhraseSet <$> lyrics t
    where buildPhraseSet x = PhraseLine x (relatedPhrase x)

collectPhrases :: WN ([Track] -> [PhraseSet])
collectPhrases trax = pairLyrics <$> trax `using` parList rseq

loadPhraseSets :: IO [PhraseSet]
loadPhraseSets = do
  trax <- parseDirectory "ross"
  run collectPhrases trax
