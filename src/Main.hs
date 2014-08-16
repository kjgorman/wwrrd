{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.ByteString.Char8 as B (unpack, pack)
import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Control.Applicative
import           Data.Aeson (encode)
import qualified Data.Set as S
import           PhraseSet
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           WWRRD
import           RedisClient

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
    route [ ("query/:query", lookupHandler), ("phrases", phraseHandler) ] <|>
    dir "static" (serveDirectory "./static/") <|>
    dir "out" (serveDirectory "./out/")

phraseHandler :: Snap ()
phraseHandler = do
  phraseSet <- liftIO $ readPhrasesFromStore
  let lines = concatMap phraseLines phraseSet
  writeBS $ B.pack . show . S.unions $ map phrases lines

lookupHandler :: Snap ()
lookupHandler = do
  queryText <- getParam "query"
  case queryText of
    Nothing -> writeBS "Please include a query on the URL"
    (Just text) -> getPhraseLine . words . B.unpack $ text

getPhraseLine :: [String] -> Snap ()
getPhraseLine lns = do
  phrase <- liftIO $ findPhrasesFor lns >>= most
  writeBS $ (B.pack . show . encode) phrase

findPhrasesFor :: [String] -> IO [PhraseSet]
findPhrasesFor lns = do
  phraseResponse <- liftIO $ readPhrasesFromStore
  findRelatedPhrases lns phraseResponse
