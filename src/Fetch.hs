{-# LANGUAGE OverloadedStrings #-}
-- | This file will make the connection to the twitter API and call various end points to fecth the data requested
module Fetch (searchTweetsByKeyWord,searchUserID) where

import Control.Exception (try)
import Data.Aeson (FromJSON, Value, encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.Text as DT
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Simple
import System.Exit (exitFailure)

-- | Bearer token for Twitter API V2 connection
bearerToken ::
  ByteString
bearerToken = "Bearer AAAAAAAAAAAAAAAAAAAAALkQjwEAAAAAYzoaAxoOqmBG8RZOxGIsXMU3l9g%3DhNsDTdFbb3SGglEV6ofgoXdi8MhFk1I3cjSMF8gTxePz1tBssc"

-- |This function allows a user to enter a keyword they want to search for in Tweets made by users.
-- The API is called, and finds the most recent 10 tweets that have been made which contain the key word. It then submits these 10 records to the DB. 
-- The function also stores the key word search term the user has entered which resulted in the records. 
searchTweetsByKeyWord ::
  [Char] ->  IO S8.ByteString
searchTweetsByKeyWord query = do
  case query of
    "" -> do
      Prelude.putStrLn "Enter a key word"
      exitFailure
      return "Err"
    otherwise -> do
      let firstWord = Prelude.head query
      let first' = case firstWord of
            '#' -> "%23"
            otherwise -> [firstWord]
      let queryParams = first' ++ Prelude.tail query
      let addParams = " lang:en &expansions=author_id&tweet.fields=created_at"
      let slug = queryParams ++ addParams
      request' <- parseRequest $ "GET https://api.twitter.com/2/tweets/search/recent?query=" ++ slug
      let request =
            addRequestHeader "Authorization" bearerToken $
              setRequestSecure True $
                setRequestPort
                  443
                  request'
      response <- httpLBS request
      let apiStatCode = getResponseStatusCode response
      case apiStatCode of
        200 -> do
          return $ getResponseBody response
        otherwise -> do
          Prelude.putStrLn "Sorry, invalid request sent"
          exitFailure
          return "Err"

-- | This function allows a user to enter a specific ID to search for a user on the Twitter app.
-- The API is called, and finds the user with the matching ID. 
searchUserID ::
  [Char] ->  IO S8.ByteString
searchUserID id = do
      request' <- parseRequest $ "GET https://api.twitter.com/2/users/" ++ id
      let request =
            addRequestHeader "Authorization" bearerToken $
              setRequestSecure True $
                  setRequestPort
                    443
                    request'
      response <- httpLBS request
      let apiStatCode = getResponseStatusCode response
      case apiStatCode of
        200 -> do
          return $ getResponseBody response
        otherwise -> do
          Prelude.putStrLn "Sorry, invalid request sent"
          exitFailure
          return "Err"