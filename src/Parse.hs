{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This file will transofrm the data fetched from the API call in a way which haskell can
module Parse
  ( 
    parsingSingleTweet,
    parsingMultipleTweets,
    parseErr,
    parseUserData,
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
mapUserData :: [Char] -> [Char]
mapUserData "user_id" = "id"
mapUserData other = other

-- |apply the above field label modiefier as option
userOptions :: Options
userOptions =
  defaultOptions
    { fieldLabelModifier = mapUserData
    }

instance FromJSON Error where
  parseJSON = JSON.genericParseJSON userOptions

-- |  JSON returned if error occurs
parseErr :: L8.ByteString -> Either String Error
parseErr json = eitherDecode json :: Either String Error

-- Pasrsing orginal user data recieved from twitter
instance FromJSON OriginalUser where
  parseJSON = JSON.genericParseJSON userOptions

-- | Parsing the results retuned from the search user method
parseUserData :: L8.ByteString -> Either String OriginalUser
parseUserData json = eitherDecode json :: Either String OriginalUser

instance FromJSON User where
  parseJSON = JSON.genericParseJSON userOptions

-- | Parsing user data to haskell data type after encoding JSON
parseUser :: L8.ByteString -> Either String User
parseUser json = eitherDecode json :: Either String User

mapTweetData :: [Char] -> [Char]
mapTweetData "tweets" = "data"
mapTweetData "tweet_id" = "id"
mapTweetData "tweeted_at" = "created_at"
mapTweetData "user_id" = "author_id"
mapTweetData "contents" = "text"
mapTweetData other = other

tweetOptions :: Options
tweetOptions =
  defaultOptions
    { fieldLabelModifier = mapTweetData
    }

instance FromJSON Tweet where
  parseJSON = JSON.genericParseJSON tweetOptions

parsingSingleTweet :: L8.ByteString -> Either String Tweet
parsingSingleTweet json = eitherDecode json :: Either String Tweet

instance FromJSON Tweets where
  parseJSON = JSON.genericParseJSON tweetOptions

parsingMultipleTweets :: L8.ByteString -> Either String Tweets
parsingMultipleTweets json = eitherDecode json :: Either String Tweets