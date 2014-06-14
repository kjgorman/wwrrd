{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Char8 as B (unpack, pack)
import           Control.DeepSeq
import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Control.Applicative
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
    route [ ("query/:query", lookupHandler) ] <|>
    dir "static" (serveDirectory ".")

lookupHandler :: Snap ()
lookupHandler = do
  queryText <- getParam "query"
  case queryText of
    Nothing -> writeBS "Please include a query on the URL"
    (Just text) -> undefined

getPhrases :: IO [PhraseSet]
getPhrases = liftM (either (const []) id) readPhrasesFromStore

findRelatedPhrases :: String -> IO [PhraseSet]
findRelatedPhrases text = do
  env <- wnEnv
  phrases <- getPhrases
  let related = collectRelations env text phrases
  related `deepseq` closeEnv env
  return related

