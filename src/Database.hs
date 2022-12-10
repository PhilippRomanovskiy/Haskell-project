{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
--  This file contains functionality which will initialise the DB, create the tables, write data to the DB and read data back. 
module Database (initialiseDB, submitSingleTweet, submitMultipleTweet, findTweets, printIdAndContents, findTweetById) where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Text as DAT
import qualified Data.Char
import Data.Dynamic
import qualified Data.Text as DT
import qualified Data.Text.Lazy.IO as I
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Database.SQLite.Simple.Internal
import Database.SQLite.Simple.ToField
import Database.SQLite.Simple.ToRow
import Database.SQLite.Simple.Types
import GHC.Generics (Generic)
import Fetch
import Parse
import Types

-- Tweetsdb
instance ToJSON TweetTable where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON TweetIdAndContents where
  toEncoding = genericToEncoding defaultOptions

instance FromRow TweetTable where
  fromRow = TweetTable <$> field <*> field <*> field <*> field <*> field

instance FromRow TweetIdAndContents where
  fromRow = TweetIdAndContents <$> field <*> field

--  Initilise DB and create tables
initialiseDB :: IO Connection
initialiseDB = do
  conn <- open "twitterDB.sqlite"
  execute_
    conn
    "CREATE TABLE IF NOT EXISTS tweets (\
    \tweet_id VARCHAR(50) PRIMARY KEY,\
    \user_id VARCHAR(50),\
    \tweeted_at VARCHAR(50) NULL, \
    \contents VARCHAR(250)  NULL, \
    \user_input VARCHAR(250) NULL)"
  return conn

submitSingleTweet ::
  Connection ->  [Char] ->  Tweet ->  IO ()
submitSingleTweet conn input aTweet = do
  print "Inserting record .." 
  execute conn "INSERT INTO tweets (tweet_id,user_id,tweeted_at, contents, user_input) VALUES (?,?,?,?,?)" (tweet_id aTweet, user_id aTweet, tweeted_at aTweet, contents aTweet, input)

-- | Calls submitSingleTweet recursively for each item in list
submitMultipleTweet ::
  Connection ->  [Char] ->  [Tweet] ->  IO ()
submitMultipleTweet conn input tweets = mapM_ (submitSingleTweet conn input) tweets

--  exports results from tweets table
findTweets ::
  Connection ->  IO ()
findTweets conn = do
  fetchTweets <- query_ conn "SELECT * FROM tweets" :: IO [TweetTable]
  let results = DAT.encodeToLazyText fetchTweets
  I.writeFile "ExportedTweets.json" results

--  print tweet_id and contents
printIdAndContents ::
  Connection ->  IO ()
printIdAndContents conn = do
  fetchTweets <- query_ conn "SELECT user_id, contents FROM tweets" :: IO [TweetIdAndContents]
  let results = DAT.encodeToLazyText fetchTweets
  print results

--  print tweet by tweet_id
findTweetById ::
  Connection -> String -> IO ()
findTweetById conn id = do
  let sql = "SELECT * FROM tweets where tweet_id = " <> id
  print sql
  -- fetchTweets <- query_ conn (IsString  id) :: IO [TweetTable]
  -- let results = DAT.encodeToLazyText fetchTweets
  -- print fetchTweets
  -- I.writeFile "tweets.json" results