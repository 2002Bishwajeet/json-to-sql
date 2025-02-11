module Main (main) where

import Control.Monad (unless)
import Data.Maybe
import Data.Time.Clock (diffUTCTime, getCurrentTime)
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
      let table = fromMaybe (takeBaseName file) tableNameArg
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
  -- Clear the whitespace at the end of document if present
  let content' = reverse $ dropLeading $ reverse content
   in -- if content is empty
      -- Parse the JSON
      case parseJsonValue content' of
        Just (json, "") -> return $ Just json
        _ -> error "Invalid JSON"

convertToSql :: Config -> JsonValue -> String
convertToSql config json = do
  let isOnlyJsonArray = isOnlyJsonArrayObject json
  if isOnlyJsonArray
    then
      let firstObject = head (fromJust (jsonValueToArray json))
          createTableSql = createTable (tableName config) firstObject
          insertTableSqlStatements = map (insertTable (tableName config)) (fromJust (jsonValueToArray json))
       in createTableSql ++ "\n\nBEGIN TRANSACTION;\n" ++ unlines insertTableSqlStatements ++ "COMMIT;"
    else
      let createTableSql = createTable (tableName config) json
          insertTableSql = insertTable (tableName config) json
       in createTableSql ++ "\n\n" ++ insertTableSql

main :: IO ()
main = do
  start <- getCurrentTime
  config <- parseArgs
  json <- readAndParseJson (filepath config)
  _ <- case json of
    Just j -> do
      let sql = convertToSql config j
          outputFile = destPath config ++ "/" ++ tableName config ++ ".sql"
      writeFile outputFile sql
      putStrLn $ "SQL file created successfully: " ++ outputFile
    _ -> error "Invalid JSON"
  end <- getCurrentTime
  let diff = diffUTCTime end start
  putStrLn $ "Execution time: " ++ show diff
