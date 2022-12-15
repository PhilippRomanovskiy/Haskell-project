{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This file will transofrm the data fetched from the API call in a way which haskell can
module Parse
  ( 
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
import Distribution.SPDX (LicenseId (JSON))
import Distribution.Simple (License (BSD2))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char
import qualified Distribution.Fields as JSON
import GHC.Generics
import Types

-- | Renames variables from JSON into ones defined in types.hs
mapUserData :: [Char] -> [Char]
mapUserData "error_data" = "errors"
mapUserData "meta_error" = "meta"
mapUserData "raw_user_data" = "data"
mapUserData "user_id" = "id"
mapUserData other = other

-- |apply the above field label modiefier as option
customOptionsUser :: Options
customOptionsUser =
  defaultOptions
    { fieldLabelModifier = mapUserData
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

mapTweetData :: [Char] -> [Char]
mapTweetData "tweets" = "data"
mapTweetData "tweet_id" = "id"
mapTweetData "tweeted_at" = "created_at"
mapTweetData "fk_user_id" = "author_id"
mapTweetData "contents" = "text"
mapTweetData other = other

customOptionsTweet :: Options
customOptionsTweet =
  defaultOptions
    { fieldLabelModifier = mapTweetData
    }

instance FromJSON Tweet where
  parseJSON = JSON.genericParseJSON customOptionsTweet

parsingSingleTweet :: L8.ByteString -> Either String Tweet
parsingSingleTweet json = eitherDecode json :: Either String Tweet

instance FromJSON Tweets where
  parseJSON = JSON.genericParseJSON customOptionsTweet

parsingMultipleTweets :: L8.ByteString -> Either String Tweets
parsingMultipleTweets json = eitherDecode json :: Either String Tweets