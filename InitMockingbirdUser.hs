{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.PostgreSQL.Simple

owl_conn :: BS.ByteString
owl_conn = "host=localhost port=5432 user=cutsea110 password=cutsea110 dbname=owl"
mock_conn :: BS.ByteString
mock_conn = "host=localhost port=5432 user=cutsea110 password=cutsea110 dbname=mockingbird"

collect :: NominalDiffTime -> IO [(Text, Text, Text)]
collect n = do
  now <- fmap (addUTCTime (-60 * n)) getCurrentTime
  conn <- connectPostgreSQL owl_conn
  query_ conn "select \"name\",\"familyname\",\"givenname\" from \"user\" where \"verstatus\"='Verified'"
    
update :: [(Text, Text, Text)] -> IO ()
update us = do
  mcon <- connectPostgreSQL mock_conn
  forM_ us $ \(id', fn, gn) -> do
    mupdate mcon (id', fn, gn)
  where
    mupdate :: Connection -> (Text, Text, Text) -> IO ()
    mupdate c (i, f, g) = do
      execute c "insert into \"user\"(ident,family_name,given_name) values (?,?,?)" (i, f, g)
      return ()

main :: IO ()
main = collect 10 >>= update
