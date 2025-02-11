module JsonParser
  ( parseJsonString,
    parseJsonNumber,
    parseJsonBoolOrNull,
    parseJsonObject,
    parseJsonValue,
    parseKeyValuePairs,
    parseKeyValuePair,
    parseJsonArray,
    parseArrayElements,
  )
where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Debug.Trace
import Json (JsonValue (..))
import Utils (countDots, dropLeading)

-- | Parse a JsonString from a string and return the parsed JsonValue and the rest of the string
parseJsonString :: String -> Maybe (JsonValue, String)
parseJsonString ('"' : xs) = parseStringContent xs ""
  where
    parseStringContent :: String -> String -> Maybe (JsonValue, String)
    parseStringContent [] _ = Nothing
    parseStringContent ('\\' : '\"' : rest) acc = parseStringContent rest (acc ++ "\"")
    parseStringContent ('\\' : '\\' : rest) acc = parseStringContent rest (acc ++ "\\")
    parseStringContent ('\\' : 'n' : rest) acc = parseStringContent rest (acc ++ "\n")
    parseStringContent ('\\' : 't' : rest) acc = parseStringContent rest (acc ++ "\t")
    parseStringContent ('"' : rest) acc = Just (JsonString acc, dropLeading rest)
    parseStringContent (c : rest) acc = parseStringContent rest (acc ++ [c])
parseJsonString _ = Nothing

parseJsonNumber :: String -> Maybe (JsonValue, String)
parseJsonNumber str =
  let (numStr, rest) = span (\c -> isDigit c || c == '.' || c == '-') str
   in if null numStr || numStr == "-" || countDots numStr > 1 -- Handle multiple dots
        then Nothing
        else
          Just
            ( if '.' `elem` numStr
                then JsonDouble (read numStr)
                else JsonInt (read numStr),
              rest
            )

parseJsonBoolOrNull :: String -> Maybe (JsonValue, String)
parseJsonBoolOrNull str
  | "true" `isPrefixOf` str = Just (JsonBool True, drop 4 str)
  | "True" `isPrefixOf` str = Just (JsonBool True, drop 4 str)
  | "false" `isPrefixOf` str = Just (JsonBool False, drop 5 str)
  | "False" `isPrefixOf` str = Just (JsonBool False, drop 5 str)
  | "null" `isPrefixOf` str = Just (JsonNull, drop 4 str)
  | "Null" `isPrefixOf` str = Just (JsonNull, drop 4 str)
  | otherwise = Nothing

parseJsonObject :: String -> Maybe (JsonValue, String)
parseJsonObject ('{' : xs) =
  let (keyValuePairs, rest) = parseKeyValuePairs xs
   in if null keyValuePairs
        then Nothing -- If the object is empty or invalid, return Nothing
        else Just (JsonObject keyValuePairs, rest)
parseJsonObject _ = Nothing

parseKeyValuePairs :: String -> ([(String, JsonValue)], String)
parseKeyValuePairs str =
  case dropLeading str of
    '}' : rest -> ([], rest)
    _ ->
      case parseKeyValuePair str of
        Just ((key, value), rest) ->
          case dropLeading rest of
            ',' : rest' ->
              let (keyValuePairs, rest'') = parseKeyValuePairs rest'
               in ((key, value) : keyValuePairs, rest'')
            '}' : rest' -> ([(key, value)], rest')
            _ -> trace "Failed to parse key-value" ([(key, value)], rest)
        Nothing -> trace "Failed to parse key-value pair" ([], str)

parseKeyValuePair :: String -> Maybe ((String, JsonValue), String)
parseKeyValuePair str =
  case parseJsonString $ dropLeading str of
    Just (JsonString key, rest) ->
      case dropLeading rest of
        ':' : rest' ->
          case parseJsonValue $ dropLeading rest' of
            Just (value, rest'') -> Just ((key, value), rest'')
            Nothing -> Nothing
        _ -> Nothing
    _ -> Nothing

parseJsonArray :: String -> Maybe (JsonValue, String)
parseJsonArray ('[' : xs) =
  case parseArrayElements xs of
    Just (elements, rest) -> Just (JsonArray elements, rest)
    Nothing -> Nothing
parseJsonArray _ = Nothing

parseArrayElements :: String -> Maybe ([JsonValue], String)
parseArrayElements str =
  case dropLeading str of
    ']' : rest -> Just ([], rest)
    _ ->
      case parseJsonValue $ dropLeading str of
        Just (value, rest) ->
          case dropLeading rest of
            ',' : rest' ->
              case parseArrayElements rest' of
                Just (values, rest'') -> Just (value : values, rest'')
                Nothing -> Nothing
            ']' : rest' -> Just ([value], rest')
            _ -> Nothing -- If ']' is missing, return an empty list and the original string
        Nothing -> Nothing

-- | Parse a JsonValue from a string and return the parsed JsonValue and the rest of the string
parseJsonValue :: String -> Maybe (JsonValue, String)
parseJsonValue str =
  case parseJsonString str of
    Just (jsonValue, rest) -> Just (jsonValue, rest)
    Nothing ->
      case parseJsonNumber str of
        Just (jsonValue, rest) -> Just (jsonValue, rest)
        Nothing ->
          case parseJsonBoolOrNull str of
            Just (jsonValue, rest) -> Just (jsonValue, rest)
            Nothing ->
              case parseJsonObject str of
                Just (jsonValue, rest) -> Just (jsonValue, rest)
                Nothing ->
                  case parseJsonArray str of
                    Just (jsonValue, rest) -> Just (jsonValue, rest)
                    Nothing -> Nothing
