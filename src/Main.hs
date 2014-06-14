{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Char8 as B (unpack, pack)
import           Control.DeepSeq
import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Control.Applicative
import           Data.Aeson (encode)
import           Data.Maybe (maybe)
import           Pick
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           WWRRD
import           RedisClient (haveCached, writePhrasesToStore, readPhrasesFromStore)

main :: IO ()
main = do
  print "booting up"
  alreadyLoaded <- haveCached
  if alreadyLoaded then quickHttpServe site
                   else loadAndServe

loadAndServe :: IO ()
loadAndServe = do
  print "loading relations to redis"
  (phrases, env) <- loadPhraseSets
  writePhrasesToStore phrases
  print "written to redis"
  closeEnv env
  print "up on 8080"
  quickHttpServe site

site :: Snap ()
site =
    ifTop (serveFile "./static/index.html") <|>
    route [ ("query/:query", lookupHandler) ] <|>
    dir "static" (serveDirectory "./static/")

lookupHandler :: Snap ()
lookupHandler = do
  queryText <- getParam "query"
  case queryText of
    Nothing -> writeBS "Please include a query on the URL"
    (Just text) -> getLines $ words $ B.unpack text

getLines :: [String] -> Snap ()
getLines lns = do
  relatedPhrases <- liftIO $ concatMapM findRelatedPhrases lns
  phrase <- liftIO $ pick relatedPhrases
  writeBS $ maybe "huh?" (B.pack . show . encode ) phrase

getPhrases :: IO [PhraseSet]
getPhrases = liftM (either (const []) id) readPhrasesFromStore

-- how does this not exist in control.monad?
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = liftM concat $ mapM f xs

findRelatedPhrases :: String -> IO [PhraseSet]
findRelatedPhrases text = do
  env <- wnEnv
  phrases <- getPhrases
  let related = collectRelations env text phrases
  related `deepseq` closeEnv env
  return related
