{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.JSON
  ( Person(..)
  , Friend(..)
  , parse
  )
where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.Time.Extra
import GHC.Generics (Generic)

data Person
  = Person
  { _id :: String
  , index :: Int
  , guid :: String
  , isActive :: Bool
  , balance :: String
  , picture :: String
  , age :: Int
  , eyeColor :: String
  , name :: String
  , gender :: String
  , company :: String
  , email :: String
  , address :: String
  , about :: String
  , registered :: String
  , latitude :: Double
  , longitude :: Double
  , tags :: [String]
  , friends :: [Friend]
  } deriving (Show, Eq, Read, Generic)

instance FromJSON Person
instance ToJSON Person

data Friend
  = Friend
  { id :: Int
  , name :: String
  } deriving (Show, Eq, Read, Generic)

instance FromJSON Friend
instance ToJSON Friend

parse :: FromJSON a => FilePath -> IO (a, POSIXTime)
parse file = do
  (bs, duration) <- diff $ BS.readFile file
  diff $ case eitherDecode bs of
    Right a   -> return a
    Left  err -> error $ "Failed to parse " ++ file ++ ": " ++ show err
