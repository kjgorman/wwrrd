{-# LANGUAGE OverloadedStrings #-}
module RedisClient (
    writePhrasesToStore
  , readPhrasesFromStore
  , haveCached
  ) where

import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import           Data.Either (either, rights)
import           Data.Maybe (maybeToList)
import qualified Data.Set as S
import           Database.Redis
import           PhraseSet
import           Track
import           WWRRD

writePhrasesToStore :: [PhraseSet] -> IO [Either Reply Status]
writePhrasesToStore phrases = do
  conn <- connect defaultConnectInfo
  runRedis conn $ mapM writePhrase phrases
    where
      writePhrase p = set (B.pack . title $ track p) $ BL.toStrict $ encode p

readPhrasesFromStore :: IO (Either String [PhraseSet])
readPhrasesFromStore = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    fetchKeys <- keys "*"
    let eitherKeys = errorsToString fetchKeys
    values <- valuesForKeys $ getKeys eitherKeys
    return $ case eitherKeys of
      Left err -> Left err
      Right _ -> Right $ valuesAsPhrases values

valuesAsPhrases :: [B.ByteString] -> [PhraseSet]
valuesAsPhrases vals = vals >>= maybeToList . Data.Aeson.decode . BL.fromStrict

getKeys :: Either String [B.ByteString] -> [B.ByteString]
getKeys = either (const []) id

errorsToString :: Either Reply [B.ByteString] -> Either String [B.ByteString]
errorsToString arg = case arg of
  Left err -> Left $ show err
  Right ks -> Right ks

valuesForKeys :: [B.ByteString] -> Redis [B.ByteString]
valuesForKeys keys = do
  perhapsVals <- mapM gets keys
  return $ rights perhapsVals >>= maybeToList

gets :: B.ByteString -> Redis (Either Reply (Maybe B.ByteString))
gets = get

haveCached :: IO Bool
haveCached = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    allKeys <- keys "*"
    return $ not $ null (getKeys . errorsToString $ allKeys)
