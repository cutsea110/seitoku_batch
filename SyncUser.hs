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
bisocie_conn :: BS.ByteString
bisocie_conn = "host=localhost port=5432 user=cutsea110 password=cutsea110 dbname=bisocie"
mock_conn :: BS.ByteString
mock_conn = "host=localhost port=5432 user=cutsea110 password=cutsea110 dbname=mockingbird"

collect :: NominalDiffTime -> IO [(Text, Text, Text, Maybe Text, Maybe Text)]
collect n = do
  now <- fmap (addUTCTime (-60 * n)) getCurrentTime
  conn <- connectPostgreSQL owl_conn
  query conn "select \"name\",\"familyname\",\"givenname\",\"email\",\"verstatus\" from \"user\" where updated > ? " (Only now)
    
update :: [(Text, Text, Text, Maybe Text, Maybe Text)] -> IO ()
update us = do
  bcon <- connectPostgreSQL bisocie_conn
  mcon <- connectPostgreSQL mock_conn
  forM_ us $ \(id', fn, gn, em, vs) -> do
    bupdate bcon (fn, gn, em, vs, id')
    mupdate mcon (fn, gn, em, id')
  where
    bupdate :: Connection -> (Text, Text, Maybe Text, Maybe Text, Text) -> IO ()
    bupdate c (f, g, e, v, i) = do
      case (e, v) of
        (Just e', Just "Verified") -> do
          execute c "update \"User\" set \"familyName\"=?,\"givenName\"=?,\"email\"=? where ident=?" (f, g, e', i)
        _ -> do
          execute c "update \"User\" set \"familyName\"=?,\"givenName\"=? where ident=?" (f, g, i)
      return ()
    mupdate :: Connection -> (Text, Text, Maybe Text, Text) -> IO ()
    mupdate c (f, g, e, i) = do
      let e' = maybe "nobody@example.net" id e
      execute c "update \"user\" set \"family_name\"=?,\"given_name\"=?,\"email\"=? where ident=?" (f, g, e', i)
      return ()

main :: IO ()
main = collect 10 >>= update
