{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSON
  ( Person(..)
  , Friend(..)
  , parseAeson
  , parseParsec
  )
where

import Control.Applicative
import Data.Aeson
import Data.Functor (void)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Time.Extra
import GHC.Generics (Generic)
import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator

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
  , phone :: String
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

parseAeson :: FromJSON a => BL.ByteString -> a
parseAeson bs = case eitherDecode bs of
  Right a   -> a
  Left  err -> error $ "Failed to parse json: " ++ show err

parseParsec :: BS.ByteString -> [Person]
parseParsec bs = case eitherResult (parse parser bs) of
  Left  err -> error $ show err
  Right a   -> a
 where
  parser :: Parser [Person]
  parser       = char '[' >> personParser `sepBy` char ',' <* char ']'

  personParser = do
    char '{'
    _id        <- parseField "_id" parseStr
    index      <- parseField "index" parseNumber
    guid       <- parseField "guid" parseStr
    isActive   <- parseField "isActive" parseBoolean
    balance    <- parseField "balance" parseStr
    picture    <- parseField "picture" parseStr
    age        <- parseField "age" parseNumber
    eyeColor   <- parseField "eyeColor" parseStr
    name       <- parseField "name" parseStr
    gender     <- parseField "gender" parseStr
    company    <- parseField "company" parseStr
    email      <- parseField "email" parseStr
    phone      <- parseField "phone" parseStr
    address    <- parseField "address" parseStr
    about      <- parseField "about" parseStr
    registered <- parseField "registered" parseStr
    latitude   <- parseField "latitude" parseNumber
    longitude  <- parseField "longitude" parseNumber
    tags       <- parseField "tags" $ arrayParser parseStr
    friends    <- parseLastField "friends" $ arrayParser friendParser
    char '}'
    return Person { .. }

  friendParser :: Parser Friend
  friendParser = do
    char '{'
    id   <- parseField "id" parseNumber
    name <- parseLastField "name" parseStr
    char '}'
    return Friend { .. }

parseField label parser = do
  parseLabel label
  res <- parser
  char ','
  return res

parseLastField label parser = do
  parseLabel label
  parser

parseStr :: Parser String
parseStr = between (char '\"') (char '\"') (many $ notChar '\"' <|> try (string "\"\"" >> return '"'))

between left right p = do
  left
  res <- p
  right
  return res

spaces = many space

parseLabel :: BS.ByteString -> Parser ()
parseLabel label = do
  spaces
  between (char '\"') (char '\"') $ string label
  spaces >> char ':' >> spaces
  return ()

parseNumber :: (Num a, Read a) => Parser a
parseNumber = do
  spaces
  digits <- many (digit <|> oneOf ".-")
  spaces
  return $ read digits

parseBoolean :: Parser Bool
parseBoolean = do
  spaces
  bool <- string "true" <|> string "false"
  spaces
  return $ bool == "true"

arrayParser :: Parser a -> Parser [a]
arrayParser parser = char '[' >> parser `sepBy` char ',' <* char ']'

oneOf :: String -> Parser Char
oneOf []       = fail "Empty list"
oneOf (x : xs) = foldl (\acc e -> acc <|> char e) (char x) xs
