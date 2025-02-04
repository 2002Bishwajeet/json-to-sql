module SqlConverter (createTable, insertTable, parseColNames) where

import Data.List (intercalate)
import Json (JsonValue (..), jsonValueToSqlType, jsonValueToString)
import Utils (sanitize)

-- | Convert a JSON string to a SQL string
createTable :: String -> JsonValue -> String
createTable tablename json = "CREATE TABLE IF NOT EXISTS " ++ sanitize tablename ++ "(\n" ++ parseColNames json ++ ");"

insertTable :: String -> JsonValue -> String
insertTable tablename (JsonObject obj) = "INSERT INTO " ++ sanitize tablename ++ " VALUES (" ++ colValues ++ ");"
  where
    colValues = intercalate ", " (map (jsonValueToString . snd) obj)
insertTable _ _ = error "Expected Json Object"

parseColNames :: JsonValue -> String
parseColNames (JsonObject obj) = intercalate ",\n" (map keyValueToString obj) -- intercalate add stuff between the list
  where
    keyValueToString (k, v) =
      let columnName = sanitize k
          columnType = jsonValueToSqlType v
       in if columnName == "id" -- make id primary key if it exists
            then columnName ++ " " ++ columnType ++ " PRIMARY KEY"
            else columnName ++ " " ++ columnType
parseColNames _ = error "Expected a JSON object"
