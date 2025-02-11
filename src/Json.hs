module Json (JsonValue (..), jsonValueToString, jsonValueToSqlType, isJsonObject, isOnlyJsonArrayObject, jsonValueToArray) where

import Data.List (intercalate)

data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonInt Int
  | JsonDouble Double
  | JsonString String
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

jsonValueToString :: JsonValue -> String
jsonValueToString JsonNull = "NULL"
jsonValueToString (JsonBool b) = if b then "TRUE" else "FALSE"
jsonValueToString (JsonInt n) = show n
jsonValueToString (JsonDouble d) = show d
jsonValueToString (JsonString s) = "\"" ++ escapeSingleQuotes s ++ "\""
jsonValueToString (JsonArray a) = "\"" ++ jsonArrayToSqlString a ++ "\""
jsonValueToString (JsonObject o) = "\"" ++ jsonObjectToSqlString o ++ "\""

escapeSingleQuotes :: String -> String
escapeSingleQuotes = concatMap (\c -> if c == '\"' then "\"\"" else [c])

jsonArrayToSqlString :: [JsonValue] -> String
jsonArrayToSqlString arr = "[" ++ intercalate ", " (map jsonValueToString arr) ++ "]"

jsonObjectToSqlString :: [(String, JsonValue)] -> String
jsonObjectToSqlString obj = "{" ++ intercalate ", " (map keyValueToString obj) ++ "}"
  where
    keyValueToString (k, v) = "\"" ++ escapeSingleQuotes k ++ "\": " ++ jsonValueToString v

-- Determines the column type of a JSON value
jsonValueToSqlType :: JsonValue -> String
jsonValueToSqlType JsonNull = "NULL"
jsonValueToSqlType (JsonBool _) = "BOOLEAN"
jsonValueToSqlType (JsonInt _) = "INTEGER"
jsonValueToSqlType (JsonDouble _) = "REAL"
jsonValueToSqlType (JsonString _) = "TEXT"
-- JSON arrays and objects are stored as strings -- TODO: Should we create a new table for this?
jsonValueToSqlType (JsonArray _) = "TEXT"
jsonValueToSqlType (JsonObject _) = "TEXT"

-- Function to check if json objects contains a single key value pair of  "data" : [array of objects]
isOnlyJsonArrayObject :: JsonValue -> Bool
-- isOnlyJsonArrayObject (JsonArray arr) = all (\v -> case v of JsonObject _ -> True; _ -> False) arr -- we can also do this
isOnlyJsonArrayObject (JsonArray arr) = all isJsonObject arr
isOnlyJsonArrayObject (JsonObject obj) = case obj of
  [(_, JsonArray arr)] -> all isJsonObject arr
  _ -> False
isOnlyJsonArrayObject _ = False

-- Check if the json value is an object
isJsonObject :: JsonValue -> Bool
isJsonObject (JsonObject _) = True
isJsonObject _ = False

jsonValueToArray :: JsonValue -> Maybe [JsonValue]
jsonValueToArray (JsonArray arr) = Just arr
jsonValueToArray (JsonObject obj) =
  if isOnlyJsonArrayObject (JsonObject obj)
    then
      ( case obj of
          [(_, JsonArray arr)] -> Just arr
          _ -> Nothing
      )
    else Nothing
jsonValueToArray _ = Nothing