module Main (main) where

import System.IO
import Types
import Fetch
import Parse
import Database


packStr'' :: String -> BS.ByteString
packStr'' = encodeUtf8 . DT.pack

main = do
  putStrLn "---------------------------------"
  putStrLn "  Welcome to the Twitter data app "
  putStrLn "  (1) Search tweets by keyword     "
  putStrLn "  (2) Get tweet by ID            "
  putStrLn "  (3) Get user by ID             "
  putStrLn "  (4) Get user information       "
  putStrLn "  (5) Export database to CSV    "
  putStrLn "  (6) Quit                       "
  putStrLn "---------------------------------"
  conn <- initDB
  hSetBuffering stdout NoBuffering
  putStr "Choose an option > "
  option <- readLn :: IO Int

