{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Char8 as B (unpack, pack)
import           Control.Monad.IO.Class
import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           WWRRD

main :: IO ()
main = quickHttpServe site

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
      logError "opening phrase dictionary"
      (phrases, env) <- liftIO loadPhraseSets
      logError "collecting relations"
      let related = collectRelations env (B.unpack text) phrases
      writeBS $ pack $ related >>= (>>= line)
      liftIO $ closeEnv env
