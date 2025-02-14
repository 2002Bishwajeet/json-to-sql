module Main (main) where

import Control.Monad (unless, when)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust, fromMaybe)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import Lib
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.FilePath (takeBaseName, takeExtension)

data Config = Config
  { filepath :: String,
    tableName :: String,
    normalize :: Bool,
    destPath :: String
  }
  deriving (Show)

data Arguments = Arguments
  { argFile :: Maybe String,
    argTable :: Maybe String,
    argNormalize :: Bool,
    argDest :: Maybe String,
    argHelp :: Bool,
    argVersion :: Bool
  }
  deriving (Show)

-- Parse individual argument
parseArgument :: String -> Arguments -> Arguments
parseArgument arg args
  | "--file=" `isPrefixOf` arg = args {argFile = Just (drop 7 arg)}
  | "--table=" `isPrefixOf` arg = args {argTable = Just (drop 8 arg)}
  | "--dest=" `isPrefixOf` arg = args {argDest = Just (drop 7 arg)}
  | "--normalize" == arg = args {argNormalize = True}
  | "-h" == arg || "--help" == arg = args {argHelp = True}
  | "-v" == arg || "--version" == arg = args {argVersion = True}
  | otherwise = case (argFile args, argTable args, argDest args) of
      (Nothing, _, _) -> args {argFile = Just arg}
      (Just _, Nothing, _) -> args {argTable = Just arg}
      (Just _, Just _, Nothing) -> args {argDest = Just arg}
      _ -> args

-- Parse all arguments
argumentParser :: [String] -> Arguments
argumentParser args = foldl (flip parseArgument) defaultArgs args
  where
    defaultArgs = Arguments Nothing Nothing False Nothing False False

-- Parse configuration from arguments
parseConfigs :: Arguments -> IO (Maybe Config)
parseConfigs args = do
  case argFile args of
    Just file -> do
      let tableNameArg = argTable args
          normalizeFlag = argNormalize args
          destArg = fromMaybe "." (argDest args)
      let table = fromMaybe (takeBaseName file) tableNameArg
      return $
        Just
          Config
            { filepath = file,
              tableName = table,
              normalize = normalizeFlag,
              destPath = destArg
            }
    Nothing -> do
      putStrLn "Usage: json-to-sql <file> [table] [dest] [--normalize]"
      return Nothing

-- Read and parse JSON file
readAndParseJson :: FilePath -> IO (Maybe JsonValue)
readAndParseJson file = do
  fileExists <- doesFileExist file
  unless fileExists $ error "File does not exist"
  let extension = takeExtension file
  unless (extension == ".json") $ error "File must have .json extension"
  content <- readFile file
  let content' = reverse $ dropLeading $ reverse content
  case parseJsonValue content' of
    Just (json, "") -> return $ Just json
    _ -> error "Invalid JSON"

-- Convert JSON to SQL
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

-- Main function
main :: IO ()
main = do
  start <- getCurrentTime
  args <- getArgs
  let parsedArgs = argumentParser args
  print parsedArgs
  when (argHelp parsedArgs) $ do
    putStrLn "Usage: json-to-sql <file> [table] [dest] [--normalize]"
    putStrLn "Options:"
    putStrLn "  --file=filename  JSON file to convert"
    putStrLn "  --table=table    Table name for the SQL file"
    putStrLn "  --dest=dest      Destination path for the SQL file"
    putStrLn "  --normalize      Normalize the JSON data"
    putStrLn "  -h, --help       Display this help message"
    putStrLn "  -v, --version    Display the version"
    exitSuccess
  when (argVersion parsedArgs) $ do
    putStrLn "json-to-sql version 0.0.1"
    exitSuccess
  maybeConfig <- parseConfigs parsedArgs
  case maybeConfig of
    Just config -> do
      json <- readAndParseJson (filepath config)
      case json of
        Just j -> do
          let sql = convertToSql config j
              outputFile = destPath config ++ "/" ++ tableName config ++ ".sql"
          writeFile outputFile sql
          putStrLn $ "SQL file created successfully: " ++ outputFile
          end <- getCurrentTime
          let diff = diffUTCTime end start
          putStrLn $ "Execution time: " ++ show diff
        _ -> error "Invalid JSON"
    Nothing -> return ()