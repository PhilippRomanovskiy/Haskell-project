{-# LANGUAGE DeriveGeneric #-}

-- | This file will transofrm the data fetched from the API call into types which haskell can interpret.
module Types
  ( Error (..),
    OriginalTweets (..),
    Tweet (..),
    Tweets (..),
    TweetTable (..),
    TweetIdAndContents (..),
  )
where

import Data.Aeson
import GHC.Generics (Generic)

--  Check error when parsing data
data Error = Error
  { error_data :: Maybe [Object],
    meta_error :: Maybe Object
  }
  deriving (Eq, Show, Generic)

newtype OriginalTweets = OriginalTweets
  { original_tweet_data :: Tweet
  }
  deriving (Eq, Show, Generic)

--  Custom data type. Haskell object relating to the data from twitter api 
data Tweet = Tweet
  { tweet_id :: String,
    user_id :: String,
    tweeted_at :: Maybe String,
    contents :: Maybe String
  }
  deriving (Eq, Show, Generic)

--  List of tweets
newtype Tweets = Tweets
  { tweets :: [Tweet]
  }
  deriving (Eq, Show, Generic)

--  Custom data type. Tweet Record object corresponding to the tweet data in the tweets table.
data TweetTable = TweetTable
  { db_tweet_id :: String,
    db_user_id :: String,
    db_tweeted_at :: Maybe String,
    db_contents :: Maybe String,
    db_user_input :: Maybe String
  }
  deriving (Eq, Show, Generic)

data TweetIdAndContents = TweetIdAndContents
  { tweet_ids :: String,
    tweet_contents :: Maybe String
  }
  deriving (Eq, Show, Generic)