{-#LANGUAGE OverloadedStrings #-}
module Track (
    Track (..)
  , parseDirectory
  ) where

import           Control.Applicative ((<*>), (<$>))
import           Control.Monad (liftM, mzero)
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Maybe
import           System.Directory

data Track = Track { media  :: Maybe String
                   , lyrics :: [String]
                   , title  :: String}
             deriving (Show)

instance FromJSON Track where
  parseJSON (Object v) = Track <$>
                         v .:? "media" <*>
                         v .: "lyrics" <*>
                         v .: "title"
  parseJSON _ = mzero

instance ToJSON Track where
  toJSON track = object ["media" .= media track
                        , "lyrics" .= lyrics track
                        , "title" .= title track]

parseFile :: FilePath -> IO (Maybe Track)
parseFile f = liftM (decode . BL.fromStrict) $ B.readFile f

parseDirectory :: FilePath -> IO [Track]
parseDirectory dir = do
  f <- getDirectoryContents dir
  files <- mapM (\f -> parseFile $ dir ++ "/" ++ f) (drop 2 f)
  return $ fromJust <$> filter isJust files
