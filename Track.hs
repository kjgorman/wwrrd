{-#LANGUAGE OverloadedStrings #-}
module Track (
    Track
  , parseDirectory
  ) where

import           Control.Applicative ((<*>), (<$>))
import           Control.Monad (liftM, mzero)
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Maybe
import           System.Directory

data Track = Track { media :: Maybe String, lyrics :: [String] }
             deriving (Show)

instance FromJSON Track where
  parseJSON (Object v) = Track <$>
                         v .:? "media" <*>
                         v .: "lyrics"
  parseJSON _ = mzero

parseFile :: FilePath -> IO (Maybe Track)
parseFile f = liftM decode $ B.readFile f

parseDirectory :: FilePath -> IO [Track]
parseDirectory dir = do
  f <- getDirectoryContents dir
  files <- mapM (\f -> parseFile $ dir ++ "/" ++ f) (drop 2 f)
  return $ map fromJust $ filter isJust files
