-- CURRENT_TIMESTAMP
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.Time.Clock
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Data = Data Int UTCTime String deriving (Show)

instance FromRow Data where
  fromRow = Data <$> field <*> field <*> field

main :: IO ()
main = withConnection "data.db" $ \conn ->
    withTransaction conn (queryAllTags ["red", "yellow"] >=> mapM_ print)

enableForeignKeySupport :: Connection -> IO ()
enableForeignKeySupport = flip execute_ "PRAGMA foreign_keys = ON"

queryAll :: Connection -> IO [Data]
queryAll conn = do
  query_ conn "SELECT id,ts,body FROM data"

queryAllTags :: [String] -> Connection -> IO [Data]
queryAllTags tags conn = query conn sql tags
  where
    sql = Query $ T.concat
      [ "SELECT d.* FROM data AS d ",
      , "INNER JOIN tag_link as t "
      , "ON t.data_id = d.id "
      , "AND t.tag_id IN "
      , "(SELECT tag.id from tag where tag.name IN (" <> qMarks <> ")) "
      , "GROUP BY d.id "
      , "HAVING COUNT(d.id) = " <> count
      ]
    -- sqlite can't parameter bind a list for the IN clause, so we
    -- generate the correct number of ?'s in order to use scalar binding
    len = length tags
    count = T.pack . show $ len
    qMarks = T.intercalate "," $ replicate len "?"


withTransaction :: Connection -> (Connection -> IO a) -> IO a
withTransaction conn act =
    E.mask $ \restore -> do
        begin
        r <- restore (act conn) `E.onException` rollback
        commit
        return r
  where
    begin = execute_ conn "BEGIN TRANSACTION"
    commit = execute_ conn "COMMIT TRANSACTION"
    rollback = execute_ conn "ROLLBACK TRANSACTION"
