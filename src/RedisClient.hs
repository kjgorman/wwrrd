{-# LANGUAGE OverloadedStrings #-}
module RedisClient (
    writePhrasesToStore
  , readPhrasesFromStore
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
import           Data.Either (either, rights)
import           Data.Maybe (maybeToList)
import qualified Data.Set as S
import           Database.Redis
import           Track
import           WWRRD

instance FromJSON PhraseSet where
  parseJSON (Object v) = PhraseSet <$>
                         v .: "track" <*>
                         v .: "line"  <*>
                         v .: "phrases"
  parseJSON _ = mzero

instance ToJSON PhraseSet where
  toJSON phrase = object [ "track"   .= (toJSON $ track phrase)
                         , "line"    .= line phrase
                         , "phrases" .= phrases phrase]

writePhrasesToStore :: [[PhraseSet]] -> IO [Either Reply Status]
writePhrasesToStore phrases = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    sequence $ map writePhrase $ concat phrases
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

errorsToString arg = case arg of
  Left err -> Left $ show err
  Right ks -> Right $ ks

valuesForKeys :: [B.ByteString] -> Redis [B.ByteString]
valuesForKeys keys = do
  perhapsVals <- sequence $ map gets keys
  return $ rights perhapsVals >>= maybeToList

gets :: B.ByteString -> Redis (Either Reply (Maybe B.ByteString))
gets = get
