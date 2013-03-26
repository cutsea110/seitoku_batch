{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Database.PostgreSQL.Simple

owl_conn :: BS.ByteString
owl_conn = "host=localhost port=5432 user=cutsea110 password=cutsea110 dbname=owl_devel"
bisocie_conn :: BS.ByteString
bisocie_conn = "host=localhost port=5432 user=cutsea110 password=cutsea110 dbname=bisocie_devel"

collect :: NominalDiffTime -> IO [(Text, Text, Text, Maybe Text, Maybe Text)]
collect n = do
  now <- fmap (addUTCTime (-60 * n)) getCurrentTime
  conn <- connectPostgreSQL owl_conn
  query conn "select \"username\",\"familyname\",\"givenname\",\"email\",\"verstatus\" from \"user\" where updated > ? " (Only now)
    
update :: [(Text, Text, Text, Maybe Text, Maybe Text)] -> IO ()
update us = do
  bcon <- connectPostgreSQL bisocie_conn
  forM_ us $ \(id', fn, gn, em, vs) -> do
    bupdate bcon (fn, gn, em, vs, id')
  where
    bupdate :: Connection -> (Text, Text, Maybe Text, Maybe Text, Text) -> IO ()
    bupdate c (f, g, e, v, i) = do
      case (e, v) of
        (Just e', Just "Verified") -> do
          execute c "update \"User\" set \"familyName\"=?,\"givenName\"=?,\"email\"=? where ident=?" (f, g, e', i)
        _ -> do
          execute c "update \"User\" set \"familyName\"=?,\"givenName\"=? where ident=?" (f, g, i)
      return ()

main :: IO ()
main = collect 10 >>= update
