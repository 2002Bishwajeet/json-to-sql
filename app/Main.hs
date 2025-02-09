module Main (main) where

import Control.Monad (unless)
import Lib
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.FilePath (takeBaseName, takeExtension)

data Config = Config
  { filepath :: String,
    tableName :: String,
    normalize :: Bool,
    destPath :: String
  }
  deriving (Show)

parseArgs :: IO Config
parseArgs = do
  args <- getArgs
  -- Check that we have at least file Args

  case args of
    (file : rest) -> do
      let tableNameArg = case rest of
            (t : _) -> Just t
            _ -> Nothing
          normalizeFlag = "--normalize" `elem` rest
          destArg = case reverse rest of
            (d : _) -> if d == "." then "." else d
            _ -> "."
      -- if No tablename provided, use the filename
      let table = maybe (takeBaseName file) id tableNameArg
      return
        Config
          { filepath = file,
            tableName = table,
            normalize = normalizeFlag,
            destPath = destArg
          }
    _ -> error "Usage: json2sql <file> [table] [--normalize] [dest]"

readAndParseJson :: FilePath -> IO (Maybe JsonValue)
readAndParseJson file = do
  -- Check if file Exists
  fileExists <- doesFileExist file
  unless fileExists $ error "File does not exist"

  -- check the valid .json extension
  let extension = takeExtension file
  unless (extension == ".json") $ error "File must have .json extension"

  -- Read the file
  content <- readFile file
  -- Clear the whitespace
  let content' = filter (`notElem` " \n\r\t") content
   in -- Parse the JSON
      case parseJsonValue content' of
        Just (json, "") -> return $ Just json
        _ -> error "Invalid JSON"

convertToSql :: Config -> JsonValue -> String
convertToSql config json =
  let createTableSql = createTable (tableName config) json
      insertTableSql = insertTable (tableName config) json
   in createTableSql ++ "\n\n" ++ insertTableSql

main :: IO ()
main = do
  config <- parseArgs
  json <- readAndParseJson (filepath config)
  case json of
    Just j -> do
      let sql = convertToSql config j
      writeFile (destPath config ++ "/" ++ tableName config ++ ".sql") sql
    _ -> error "Invalid JSON"
