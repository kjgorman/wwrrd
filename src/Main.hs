{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Char8 as B (unpack, pack)
import           Control.Monad.IO.Class
import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           WWRRD
import           RedisClient (writePhrasesToStore, readPhrasesFromStore)

main :: IO ()
main = do
  (phrases, env) <- loadPhraseSets
  writePhrasesToStore phrases
  closeEnv env
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
    (Just text) -> findRelatedPhrases text
  where
    findRelatedPhrases text = do
      env <- liftIO wnEnv
      phrases <- liftIO getPhrases
      let related = collectRelations env (B.unpack text) phrases
      liftIO $ closeEnv env
      writeBS "foo"

getPhrases :: IO [[PhraseSet]]
getPhrases = undefined
