{-#LANGUAGE ImplicitParams#-}
module WWRRD where

import Data.List
import NLP.WordNet
import Track

wnEnv :: IO WordNetEnv
wnEnv = initializeWordNetWithOptions (Just dictPath) (return warn)
        where dictPath = "dict/"
              warn s e = return () :: IO ()

similar :: (?wne :: WordNetEnv) => Word -> POS -> [SearchResult]
similar term pos = (search term pos AllSenses) >>= relatedBy Similar

main :: IO ()
main = wnEnv >>= \env -> mapM_ printWords $ runs env (similar "happy" Adj)
  where printWords s = putStrLn . show . (intercalate ",") $ srWords s AllSenses
