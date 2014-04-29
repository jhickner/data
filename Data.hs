{- TODO
[ ] Data should include [String] for tags and the query needs to return them

-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Exception as E
import Control.Monad
import Data.List (intercalate)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Time.Clock
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data Data = Data Int UTCTime String deriving (Show)
instance FromRow Data where
  fromRow = Data <$> field <*> field <*> field

data Tag = Tag String deriving (Show)
instance FromRow Tag where
  fromRow = Tag <$> field

main :: IO ()
main = withConnection "data.db" $ \conn ->
    --withTransaction conn (queryAllTags conn ["red", "yellow"]) >>= mapM_ print
    withTransaction conn (queryAll conn) >>= mapM_ print

enableForeignKeySupport :: Connection -> IO ()
enableForeignKeySupport = flip execute_ "PRAGMA foreign_keys = ON"

queryAll :: Connection -> IO [Data]
queryAll conn = query_ conn "SELECT * FROM data"

queryAllTags :: Connection -> [String] -> IO [Data]
queryAllTags conn tags = query conn sql tags
  where
    sql = Query $ T.concat
      [ "SELECT d.* FROM data AS d "
      , "INNER JOIN tag_link as t "
      , "ON t.data_id = d.id "
      , "AND t.tag_id IN "
      , "(SELECT tag.id from tag where tag.name IN (" <> qMarks <> ")) "
      , "GROUP BY d.id "
      , "HAVING COUNT(d.id) = " <> count
      ]
    -- sqlite can't parameter bind the (?,?,...) list for the IN clause, so we
    -- generate the correct number of ?'s and then bind a [String]
    len = length tags
    count = T.pack . show $ len
    qMarks = T.intercalate "," $ replicate len "?"

-- | Inserts a `Data` item along with all tags and tag_links.
-- Should be run inside a transaction!
insertData :: Connection -> [String] -> String -> IO Int
insertData conn tags body = do
    -- insert the Data and get the id
    execute conn "INSERT INTO data(body) VALUES(?)" (Only body)
    id' <- fromIntegral <$> lastInsertRowId conn

    -- get or create all the tags
    tagIds <- mapM (getOrInsertTag conn) tags

    -- insert tag_links for each data_id/tag_id combo
    mapM_ (insertTagLink conn id') tagIds

    return id'

getOrInsertTag :: Connection -> String -> IO Int
getOrInsertTag conn tag = do
    res <- query conn "SELECT id FROM tag WHERE name=?" (Only tag)
    case res of
      (Only id':_) -> return id'
      [] -> do
          execute conn "INSERT INTO tag(name) VALUES(?)" (Only tag)
          fromIntegral <$> lastInsertRowId conn

insertTagLink :: Connection -> Int -> Int -> IO ()
insertTagLink conn dataId tagId = execute conn
    "INSERT INTO tag_link(data_id,tag_id) VALUES(?,?)"
    [dataId, tagId]