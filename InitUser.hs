{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Database.PostgreSQL.Simple

owl_conn :: BS.ByteString
owl_conn = "host=localhost port=5432 user=cutsea110 password=cutsea110 dbname=owl_devel"
bisocie_conn :: BS.ByteString
bisocie_conn = "host=localhost port=5432 user=cutsea110 password=cutsea110 dbname=bisocie_devel"
collect :: IO [(Text, Maybe Text, Text, Text, Text)]
collect = do
  conn <- connectPostgreSQL bisocie_conn
  query_ conn "select \"ident\",\"password\",\"familyName\",\"givenName\",\"email\" from \"User\""
    
update :: [(Text, Maybe Text, Text, Text, Text)] -> IO ()
update us = do
  conn <- connectPostgreSQL owl_conn
  forM_ us $ \(id', mpw, fn, gn, em) -> do
    oupdate conn (id', maybe "" id mpw, fn, gn, em)
  where
    oupdate :: Connection -> (Text, Text, Text, Text, Text) -> IO ()
    oupdate c (i, p, f, g, e) = do
      execute c "insert into \"user\"(name,password,salt,familyname,givenname,email,role) values (?,?,'',?,?,?,'None')" (i, p, f, g, e)
      return ()

main :: IO ()
main = collect >>= update
