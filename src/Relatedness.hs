{-#LANGUAGE ImplicitParams#-}
{-#LANGUAGE RankNTypes #-}
module Relatedness (
                    findRelatedPhrases,
                    run,
                    relatedPhrase
                   ) where

import           Control.Applicative ((<$>))
import           Control.DeepSeq (deepseq)
import           Control.Parallel.Strategies
import           Data.Char (toLower)
import           Data.List (intersperse)
import           Data.Foldable (foldMap)
import qualified Data.Set as S
import           NLP.WordNet
import           NLP.Stemmer (stem, Stemmer(English))
import           PhraseSet

wnEnv :: IO WordNetEnv
wnEnv = initializeWordNetWithOptions (Just dictPath) (return warn)
        where dictPath = "dict/"
              warn s e = return () :: IO ()

closeEnv :: WordNetEnv -> IO ()
closeEnv = closeWordNet

allSrWords :: SearchResult -> [String]
allSrWords result = concatMap (srWords result) (srSenses result)

allPOS :: [POS]
allPOS = [Noun, Verb, Adj, Adv]

instance Show SearchResult where
    show result = concat $ intersperse "\r\n" [
     "Overview: " ++ (show $ srOverview result),
     "Sensetype: " ++ (show $ srSenseNum result),
     "POS: " ++ (show $ srPOS result),
     "Defn: " ++ (show $ srDefinition result),
     "Senses: "++ (show $ srSenses result),
     "Words: "++ (show $ allSrWords result),
     "Forms: "++ (show $ srForms result)
     ]

immediate :: WN (String -> S.Set String)
immediate w = S.fromList $ do
  pos <- allPOS
  results <- search w pos AllSenses
  allSrWords results

depth :: WN (Int -> String -> S.Set String)
depth 1 w = S.union (immediate w) (related w)
depth n w = let neighbours = depth 1 w
            in foldMap (depth (n-1)) neighbours

related ::  WN (String -> S.Set String)
related w = foldl S.union S.empty $ map (S.fromList . allSrWords) $ do
  pos <- allPOS
  result <- search w pos AllSenses
  pos <- [Antonym, Holonym, Hypernym, Meronym, Hyponym, Similar]
  relatedBy Antonym result

relatedPhrase :: WN (String -> S.Set String)
relatedPhrase w = let before = S.fromList $ words w
                      parts = map (related . stem English . downcase) $ words w
                      downcase = map toLower
                  in foldl S.union before parts

run :: NFData b => WN (a -> b) -> a -> IO b
run f x = do
  env <- wnEnv
  let result = runs env (f x)
  result `deepseq` closeEnv env
  return result

collectRelations :: WN ([PhraseSet] -> String -> [PhraseSet])
collectRelations phrases text = collectIntersecting (relatedPhrase text) phrases

collectIntersecting :: S.Set Word -> [PhraseSet] -> [PhraseSet]
collectIntersecting rel p = filter (not . null . phraseLines) intersecting
  where intersecting = intersectingPhrases rel <$> p `using` rseq

intersectingPhrases :: S.Set Word -> PhraseSet -> PhraseSet
intersectingPhrases rel (PhraseSet t phraseLines) = PhraseSet t $ notEmpty . with $ phraseLines
    where with = map (\(PhraseLine l p) -> PhraseLine l $ S.intersection rel p)
          notEmpty = filter (not . S.null . phrases)

findRelatedPhrases :: String -> [PhraseSet] -> IO [PhraseSet]
findRelatedPhrases text phraseSet = run (collectRelations phraseSet) text
