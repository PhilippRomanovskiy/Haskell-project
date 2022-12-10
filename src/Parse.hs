{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This file will transofrm the data fetched from the API call in a way which haskell can
module Parse
  ( parseDataTweet,
    parsingSingleTweet,
    parsingMultipleTweets,
    parseErr,
  )
where

import Control.Arrow
import Data.Aeson
import qualified Data.Aeson as JSON
import Data.ByteString.Internal
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import qualified Distribution.Fields as JSON
import Distribution.SPDX (LicenseId (JSON))
import Distribution.Simple (License (BSD2))
import GHC.Generics
import Types

-- | Renames variables from JSON into ones defined in types
renameFieldsUser :: [Char] -> [Char]
renameFieldsUser "error_data" = "errors"
renameFieldsUser "user_id" = "id"
renameFieldsUser other = other

-- |apply the above field label modiefier as option
customOptionsUser :: Options
customOptionsUser =
  defaultOptions
    { fieldLabelModifier = renameFieldsUser
    }

-- | Parsing errors function
instance FromJSON Error where
  parseJSON = JSON.genericParseJSON customOptionsUser

-- |  Parses the JSON file in case an error response is retuened by the function
parseErr :: L8.ByteString -> Either String Error
parseErr json = eitherDecode json :: Either String Error

-- | Remove Extra header from twitter API in some instances
jsonOptions :: String -> JSON.Options
jsonOptions prefix =
  let prefixLength = length prefix
   in JSON.defaultOptions {JSON.fieldLabelModifier = drop prefixLength >>> renameFieldsUser}

-- | Renames variables from JSON into ones defined in types
renameFieldsRaw "original_tweet_data" = "data"
renameFieldsRaw "tweeted_at" = "created_at"
renameFieldsRaw other = other

customOptionsRaw :: Options
customOptionsRaw =
  defaultOptions
    { fieldLabelModifier = renameFieldsRaw
    }

-- Parsing Raw Tweet Data
instance FromJSON OriginalTweets where
  parseJSON = JSON.genericParseJSON customOptionsRaw

--  Parses the raw data returned by the getUser funtion in fetch defined in fetch and used in main
parseDataTweet :: L8.ByteString -> Either String OriginalTweets
parseDataTweet json = eitherDecode json :: Either String OriginalTweets

renameFieldsTweet :: [Char] -> [Char]
renameFieldsTweet "tweets" = "data"
renameFieldsTweet "tweet_id" = "id"
renameFieldsTweet "tweeted_at" = "created_at"
renameFieldsTweet "user_id" = "author_id"
renameFieldsTweet "contents" = "text"
renameFieldsTweet other = other

customOptionsTweet :: Options
customOptionsTweet =
  defaultOptions
    { fieldLabelModifier = renameFieldsTweet
    }

-- Parsing  a single Tweet Data
instance FromJSON Tweet where
  parseJSON = JSON.genericParseJSON customOptionsTweet

-- | Parse a single tweet by decoding from Json to Tweet that is returned by GetTweet function defiend in fetch and used in main
parsingSingleTweet :: L8.ByteString -> Either String Tweet
parsingSingleTweet json = eitherDecode json :: Either String Tweet

-- Parsing Array of Tweet Data
instance FromJSON Tweets where
  parseJSON = JSON.genericParseJSON customOptionsTweet

-- | Parses the arrays of Tweets by decoding from Json to Tweeets returned by SearchTweets function defiend in fetch and used in main
parsingMultipleTweets :: L8.ByteString -> Either String Tweets
parsingMultipleTweets json = eitherDecode json :: Either String Tweets
