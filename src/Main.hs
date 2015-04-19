{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as B (unpack, pack)
import qualified Data.ByteString.Lazy as BL (toStrict)
import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Control.Applicative
import           Data.Aeson (encode)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import           Frequency (build, score)
import           PhraseSet
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server
import           WWRRD
import           RedisClient
import           WeightedSelection (select)

main :: IO ()
main = do
  print "booting up"
  alreadyLoaded <- haveCached
  print $ "have i loaded stuff?: " ++ show alreadyLoaded
  if alreadyLoaded then quickHttpServe site
                   else loadAndServe

loadAndServe :: IO ()
loadAndServe = do
  print "loading relations to redis"
  phrases <- loadPhraseSets
  print "loaded phrases"
  writePhrasesToStore phrases
  print "written to redis"
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
  phraseSet <- liftIO readPhrasesFromStore
  let lines = concatMap phraseLines phraseSet
  writeBS $ B.pack . show . S.unions $ map phrases lines

lookupHandler :: Snap ()
lookupHandler = do
  queryText <- getParam "query"
  case queryText of
    Nothing -> writeBS "Please include a query on the URL"
    (Just text) -> getPhraseLine . B.unpack $ text

getPhraseLine :: String -> Snap ()
getPhraseLine line = do
  counts <- liftIO build
  phrase <- liftIO $ findPhrasesFor line >>= weighted counts
  writeBS $ (BL.toStrict . encode) phrase

weighted :: M.Map String Int -> [PhraseSet] -> IO PhraseLine
weighted counts = select (toRational . weight counts . phrases) . concatMap phraseLines

weight :: Floating a => M.Map String Int -> S.Set String -> a
weight counts words = S.foldl (\s w -> s + score counts w) 0 words

findPhrasesFor :: String -> IO [PhraseSet]
findPhrasesFor line = do
  phraseResponse <- liftIO readPhrasesFromStore
  findRelatedPhrases line phraseResponse
