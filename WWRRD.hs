{-#LANGUAGE ImplicitParams#-}
module WWRRD where

import Data.List (intercalate)
import NLP.WordNet
import Track

wnEnv :: IO WordNetEnv
wnEnv = initializeWordNetWithOptions (Just dictPath) (return warn)
        where dictPath = "dict/"
              warn s e = return () :: IO ()

similar :: (?wne :: WordNetEnv) => Word -> POS -> [SearchResult]
similar term pos = (search term pos AllSenses) >>= relatedBy Similar

collectSimilar :: (?wne :: WordNetEnv) => String -> [Word]
collectSimilar str = forPos >>= similarWords
    where
      tkns = words str
      forPos = [(w, p) | p <- [Adj, Noun, Verb, Adv], w <- tkns]
      similarWords s = similar (fst s) (snd s) >>= (flip srWords $ AllSenses)


main :: IO ()
main = wnEnv >>= \env -> mapM_ printWords $ runs env (similar "sad" Adj)
  where printWords s = putStrLn . show . (intercalate ",") $ srWords s AllSenses
