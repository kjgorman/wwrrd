{-# LANGUAGE OverloadedStrings #-}
module RedisClient (
    writePhrasesToStore
  , readPhrasesFromStore
  ) where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as BL
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

writePhrasesToStore :: [[PhraseSet]] -> IO ()
writePhrasesToStore phrases = do
  conn <- connect defaultConnectInfo
  runRedis conn $ do
    sequence $ map writePhrase $ concat phrases
    return ()
    where
      writePhrase p = set (B.pack . title $ track p) $ BL.toStrict $ encode p

readPhrasesFromStore :: IO [[PhraseSet]]
readPhrasesFromStore = undefined
