{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This file will transofrm the data fetched from the API call in a way which haskell can
module Parse
  ( parseDataTweet,
    parsingSingleTweet,
    parsingMultipleTweets,
    parseErr,
    parseDataUser,
    parseUser,
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

-- | Renames variables from JSON into ones defined in types.hs
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

instance FromJSON Error where
  parseJSON = JSON.genericParseJSON customOptionsUser

-- |  JSON returned if error occurs
parseErr :: L8.ByteString -> Either String Error
parseErr json = eitherDecode json :: Either String Error

-- Pasrsing orginal user data recieved from twitter
instance FromJSON OriginalUser where
  parseJSON = JSON.genericParseJSON customOptionsUser

-- | Parsing the results retuned from the search user method
parseDataUser :: L8.ByteString -> Either String OriginalUser
parseDataUser json = eitherDecode json :: Either String OriginalUser

instance FromJSON User where
  parseJSON = JSON.genericParseJSON customOptionsUser

-- | Parsing user data to haskell data type after encoding JSON
parseUser :: L8.ByteString -> Either String User
parseUser json = eitherDecode json :: Either String User


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

instance FromJSON Tweet where
  parseJSON = JSON.genericParseJSON customOptionsTweet

parsingSingleTweet :: L8.ByteString -> Either String Tweet
parsingSingleTweet json = eitherDecode json :: Either String Tweet

instance FromJSON Tweets where
  parseJSON = JSON.genericParseJSON customOptionsTweet

parsingMultipleTweets :: L8.ByteString -> Either String Tweets
parsingMultipleTweets json = eitherDecode json :: Either String Tweets