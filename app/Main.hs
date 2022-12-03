--  This is the main file which will execute functions within the programme.
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

main = do
  putStrLn "---------------------------------"
  putStrLn "  Welcome to the Twitter data app, please select an option from the list below: "
  putStrLn "  (1) Search tweets by keyword   "
  putStrLn "  (5) Download DB to JSON        "
  putStrLn "  (6) Quit                       "
  putStrLn "---------------------------------"

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
    5 -> do
      print "Saving tweet data to ExportedTweets.json"
      findTweets conn
      main
    6 -> do
     print "Hope you've enjoyed using the app!"
     exitSuccess
    _ -> do
      exitFailure