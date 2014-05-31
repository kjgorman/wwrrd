{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Char8 as B (unpack, pack)
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

loadAndServe = do
  (phrases, env) <- loadPhraseSets
  writePhrasesToStore phrases
  print "written to redis"
  closeEnv env
  print "up on 8080"

site :: Snap ()
site =
    route [ ("query/:query", lookupHandler) ] <|>
    dir "static" (serveDirectory ".")

lookupHandler :: Snap ()
lookupHandler = do
  queryText <- getParam "query"
  case queryText of
    Nothing -> writeBS "Please include a query on the URL"
    (Just text) -> findRelatedPhrases text
  where
    findRelatedPhrases text = do
      env <- liftIO wnEnv
      phrases <- liftIO getPhrases
      let related = collectRelations env (B.unpack text) phrases
      liftIO $ closeEnv env
      writeBS . B.pack $ show related

getPhrases :: IO [PhraseSet]
getPhrases = liftM (either (const []) id) readPhrasesFromStore
