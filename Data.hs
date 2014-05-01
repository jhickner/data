{- TODO
[X] - import the existing data from mongo
[X] - reimport, adding "link" tag to any item that had a link
[X] - nice ANSI output
[X] - display the deleted item
[ ] - list tags sorted by frequency
[ ] - doesn't return all tags, only the tags used in the query

cleaning orphan tag_links (ON DELETE CASCADE handles this):
delete from tag_link where data_id in (select data_id from tag_link except select id from data);
delete from tag where id in (select id from tag except select tag_id from tag_link);

-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Control.Exception as E
import Control.Monad
--import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.List (intercalate, nub, sort)
import Data.Maybe (listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Options.Applicative
import System.Console.ANSI
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))
import System.Locale (defaultTimeLocale)

-- TYPES

data Data = Data
  { dId   :: Int
  , dTs   :: UTCTime
  , dBody :: Text
  , dTags :: [Text]
  } deriving (Show)

instance FromRow Data where
  fromRow = Data <$> field <*> field <*> field <*> (T.splitOn "," <$> field)

data Input
  = AddData String [String]
  | DeleteData Int
  | TagQuery [String]
  deriving (Show)

-- INPUT

commands :: Parser Input
commands = subparser
    ( command "add" (info addData (progDesc "Add an entry"))
   <> command "delete" (info deleteData (progDesc "Delete an entry"))
   <> command "query" (info tagQuery (progDesc "Query by tag(s)"))
    )
  where
    addData = AddData <$> (argument str (metavar "BODY"))
                      <*> many (argument str (metavar "TAG..."))
    deleteData = DeleteData <$> (argument auto (metavar "TAG ID"))
    tagQuery = TagQuery <$> many (argument str (metavar "TAG..."))

run :: Input -> IO ()
run i = case i of
    AddData body tags -> addData body tags
    DeleteData id     -> deleteData' id
    TagQuery tags     -> tagQuery tags
  where
    withConn' act = getDBPath >>= flip withConnection act
    cleanTags = sort . nub . map (T.toLower . T.pack)

    tagQuery tags = do
        res <- withConn' (flip queryAllTags (cleanTags tags))
        tz <- getCurrentTimeZone
        mapM_ (prettyPrint tz) res

    addData body tags = do
        res <- withConn' $ \conn -> withTransaction conn $ do
            insertData conn (cleanTags tags) (T.pack body) Nothing
        putStrLn $ "added entry id: " ++ show res

    deleteData' id = do
        md <- withConn' (flip getData id)
        maybe notFound ((>> delete) . display) md
      where
        notFound = putStrLn "id not not found!"
        display d = do
            tz <- getCurrentTimeZone
            prettyPrint tz d
        delete = do
            withConn' (flip deleteData id)
            putStrLn "deleted!"

getDBPath :: IO FilePath
getDBPath = (</> ".data.db") <$> getHomeDirectory

main :: IO ()
main = execParser (info (commands <**> helper) idm) >>= run

-- DISPLAY

prettyPrint :: TimeZone -> Data -> IO ()
prettyPrint tz Data{..} = do
    TIO.putStrLn $ T.concat
        [ "[ "
        , color Blue (T.justifyLeft 5 ' ' (T.pack . show $ dId))
        , " | "
        , fmtTime dTs
        , " | "
        , T.intercalate ", " (map (color Magenta) dTags)
        , " ]\n"
        , dBody
        , "\n"
        ]
  where
    fmtTime = T.pack
      . formatTime defaultTimeLocale "%b %d, %Y %I:%M %P"
      . utcToLocalTime tz

color :: Color -> Text -> Text
color c t = T.concat
    [ T.pack $ setSGRCode [SetColor Foreground Dull c]
    , t
    , T.pack $ setSGRCode [Reset]
    ]

-- DATABASE

enableForeignKeySupport :: Connection -> IO ()
enableForeignKeySupport = flip execute_ "PRAGMA foreign_keys = ON"

queryAll :: Connection -> IO [Data]
queryAll conn = query_ conn "SELECT * FROM data"

getData :: Connection -> Int -> IO (Maybe Data)
getData conn id = do
    res <- query conn sql (Only id)
    return $ listToMaybe res
  where
    sql = Query $ T.concat
      [ "SELECT d.*, GROUP_CONCAT(t.name) "
      , "FROM data AS d "
      , "INNER JOIN tag_link AS tl "
      , "ON tl.data_id = d.id "
      , "INNER JOIN tag AS t ON t.id = tl.tag_id "
      , "WHERE d.id = ?"
      , "GROUP BY d.id"
      ]

queryAllTags :: Connection -> [Text] -> IO [Data]
queryAllTags conn tags = query conn sql tags
  where
    sql = Query $ T.concat
      [ "SELECT d.*, GROUP_CONCAT(t.name) "
      , "FROM data AS d "
      , "INNER JOIN tag_link as tl "
      , "ON tl.data_id = d.id "
      , "AND tl.tag_id IN "
      , "(SELECT tag.id from tag where tag.name IN (" <> qMarks <> ")) "
      , "INNER JOIN tag AS t ON t.id = tl.tag_id "
      , "GROUP BY d.id "
      , "HAVING COUNT(d.id) = " <> count
      , " ORDER BY d.ts "
      ]
    -- sqlite can't parameter bind the (?,?,...) list for the IN clause, so we
    -- generate the correct number of ?'s and then bind a [Text]
    len = length tags
    count = T.pack . show $ len
    qMarks = T.intercalate "," $ replicate len "?"

deleteData :: Connection -> Int -> IO ()
deleteData conn id = do
  enableForeignKeySupport conn
  execute conn "DELETE FROM data WHERE id=?" (Only id)

-- | Inserts a `Data` item along with all tags and tag_links.
-- Should be run inside a transaction!
insertData :: Connection -> [Text] -> Text -> Maybe UTCTime -> IO Int
insertData conn tags body mts = do
    ts <- maybe getCurrentTime return mts

    -- insert the Data and get the id
    execute conn "INSERT INTO data(body,ts) VALUES(?,?)" (body, ts)
    id' <- fromIntegral <$> lastInsertRowId conn

    -- get or create all the tags
    tagIds <- mapM (getOrInsertTag conn) tags

    -- insert tag_links for each data_id/tag_id combo
    mapM_ (insertTagLink conn id') tagIds

    return id'

getOrInsertTag :: Connection -> Text -> IO Int
getOrInsertTag conn tag = do
    res <- query conn "SELECT id FROM tag WHERE name=?" (Only tag)
    maybe insert (return . fromOnly) (listToMaybe res)
  where
    insert = do
        execute conn "INSERT INTO tag(name) VALUES(?)" (Only tag)
        fromIntegral <$> lastInsertRowId conn

insertTagLink :: Connection -> Int -> Int -> IO ()
insertTagLink conn dataId tagId = execute conn
    "INSERT INTO tag_link(data_id,tag_id) VALUES(?,?)"
    [dataId, tagId]