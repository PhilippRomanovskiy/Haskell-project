-- |  This is the main file which will execute functions within the programme.
module Main where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Dynamic
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8)
import Data.Yaml.Builder (toByteString)
import Database
import Distribution.SPDX (LicenseId (JSON))
import Distribution.Types.InstalledPackageInfo.Lens (description)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Fetch
import Parse
import Types

packStr'' :: String -> BS.ByteString
packStr'' = encodeUtf8 . DT.pack

-- |  Menu selection for functions within the app.
main = do
  putStrLn "-----------------------------------"
  putStrLn "  Welcome to the Twitter data app, please select an option from the list below: "
  putStrLn "  (1) Search tweets by keyword     "
  putStrLn "  (2) Print user_id and contents   "
  putStrLn "  (3) Search tweets by tweet_id    "
  putStrLn "  (4) Search tweets by time        "
  putStrLn "  (5) Search twitter user by ID"
  putStrLn "  (6) Download DB to JSON          "
  putStrLn "  (7) Quit                         "
  putStrLn "-----------------------------------"

  conn <- initialiseDB
  hSetBuffering stdout NoBuffering
  putStr "Choose an option > "
  option <- readLn :: IO Int
  case option of
    1 -> do
      putStr "Key word: "
      hFlush stdout
      input <- getLine
      print "Downloading result set..."
      json <- searchTweetsByKeyWord input
      print "Parsing result set..."
      case parsingMultipleTweets json of
        Left err -> case parseErr json of
          Left err -> print err
          Right errorresult -> do
            print "No results found"
            main
        Right result -> do
          let output_tweets = tweets result
          print "Saving to DB..."
          submitMultipleTweet conn input output_tweets
          main

    2 -> do
      print "User_id and Contents"
      printIdAndContents conn
      main    

    3 -> do
      print "Find tweet by ID, please input tweet_id"
      putStr "tweet_id: "
      hFlush stdout
      input <- getLine
      -- 
      findTweetById conn input
      main

    4 -> do
      print "Find tweet by time, please input time"
      putStr "time: "
      hFlush stdout
      input <- getLine
      findTweetBytime conn input
      main

    5 -> do
      print "Try (2244994945)"
      putStr "User ID: "
      hFlush stdout
      input <- getLine
      print "Downloading..."
      json <- searchUserID input
      print "Parsing..."
      case parseDataUser json of
        Left err -> case parseErr json of
          Left err -> print err
          Right errorresult -> do
            print "Could not find user with such ID"
            main
        Right result -> do
          let output = raw_user_data result
          let output_user = User (pk_user_id output) (username output) (name output)
          print "Saving on DB..."
          submitUser conn output_user
          main

    6 -> do
      print "Saving tweet data to ExportedTweets.json"
      findTweets conn
      main

    7 -> do
      print "Hope you've enjoyed using the app!"
      exitFailure

    _ -> do
      print "Please enter a number from 1 to 5"
      main
