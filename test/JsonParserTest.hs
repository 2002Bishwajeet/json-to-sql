module JsonParserTest (tests) where

import Lib
import Test.HUnit

-- Sample test cases for parseJsonString
testParseJsonString :: Test
testParseJsonString = TestCase $ do
  assertEqual
    "parseJsonString should parse a valid string"
    (Just (JsonString "key", ": \"value\""))
    (parseJsonString "\"key\": \"value\"")
  assertEqual
    "parseJsonString should return Nothing for invalid strings"
    Nothing
    (parseJsonString "key\": \"value\"")
  assertEqual
    "parseJsonString should parse a string with escape characters"
    (Just (JsonString "key\nvalue", ""))
    (parseJsonString "\"key\\nvalue\"")
  assertEqual
    "parseJsonString should parse a string with special escape characters"
    (Just (JsonString "body", ": \" liquid. \\\"Sir, you will undoubtedly end up in a drunkard's grave,\\\"  I glared as I pushed the glass across the table.\""))
    (parseJsonString "\"body\": \" liquid. \\\"Sir, you will undoubtedly end up in a drunkard's grave,\\\"  I glared as I pushed the glass across the table.\"")

-- Sample test cases for parseJsonNumber
testParseJsonNumber :: Test
testParseJsonNumber = TestCase $ do
  assertEqual
    "parseJsonNumber should parse a valid Double"
    (Just (JsonDouble 3.14, ""))
    (parseJsonNumber "3.14")
  assertEqual
    "parseJsonNumber should return Nothing for invalid numbers"
    Nothing
    (parseJsonNumber "abc")
  assertEqual
    "parseJsonNumber should parse a valid number with trailing characters"
    (Just (JsonDouble 3.14, "abc"))
    (parseJsonNumber "3.14abc")
  assertEqual
    "parseJsonNumber should parse a valid Integer"
    (Just (JsonInt 3, ""))
    (parseJsonNumber "3")

-- Sample test cases for parseJsonObject
testParseJsonObject :: Test
testParseJsonObject = TestCase $ do
  assertEqual
    "parseJsonObject should parse a valid object"
    (Just (JsonObject [("key", JsonString "value")], ""))
    (parseJsonObject "{\"key\": \"value\"}")
  assertEqual
    "parseJsonObject should return Nothing for invalid object"
    Nothing
    (parseJsonObject "{\"key\": value}")

-- Sample test cases for parseJsonBoolOrNull
testParseJsonBoolOrNull :: Test
testParseJsonBoolOrNull = TestCase $ do
  assertEqual
    "parseJsonBoolOrNull should parse a valid true boolean"
    (Just (JsonBool True, ""))
    (parseJsonBoolOrNull "true")
  assertEqual
    "parseJsonBoolOrNull should parse a valid false boolean"
    (Just (JsonBool False, ""))
    (parseJsonBoolOrNull "false")
  assertEqual
    "parseJsonBoolOrNull should parse a valid null value"
    (Just (JsonNull, ""))
    (parseJsonBoolOrNull "null")
  assertEqual
    "parseJsonBoolOrNull should return Nothing for invalid boolean or null"
    Nothing
    (parseJsonBoolOrNull "tru")

-- Sample test cases for parseJsonArray
testParseJsonArray :: Test
testParseJsonArray = TestCase $ do
  assertEqual
    "parseJsonArray should parse a valid array"
    (Just (JsonArray [JsonInt 1, JsonInt 2, JsonInt 3], ""))
    (parseJsonArray "[1, 2, 3]")
  assertEqual
    "parseJsonArray should parse an empty array"
    (Just (JsonArray [], ""))
    (parseJsonArray "[]")
  assertEqual
    "parseJsonArray should return Nothing for invalid array"
    Nothing
    (parseJsonArray "[1, 2, 3")

-- Sample test cases for parseJsonValue
testParseJsonValue :: Test
testParseJsonValue = TestCase $ do
  assertEqual
    "parseJsonValue should parse a valid string"
    (Just (JsonString "value", ""))
    (parseJsonValue "\"value\"")
  assertEqual
    "parseJsonValue should parse a valid number"
    (Just (JsonDouble 3.14, ""))
    (parseJsonValue "3.14")
  assertEqual
    "parseJsonValue should parse a valid boolean"
    (Just (JsonBool True, ""))
    (parseJsonValue "true")
  assertEqual
    "parseJsonValue should parse a valid null"
    (Just (JsonNull, ""))
    (parseJsonValue "null")
  assertEqual
    "parseJsonValue should parse a valid object"
    (Just (JsonObject [("key", JsonString "value")], ""))
    (parseJsonValue "{\"key\": \"value\"}")
  assertEqual
    "parseJsonValue should parse a valid array"
    (Just (JsonArray [JsonInt 1, JsonInt 2, JsonInt 3], ""))
    (parseJsonValue "[1, 2, 3]")
  assertEqual
    "parseJsonValue should return Nothing for invalid value"
    Nothing
    (parseJsonValue "invalid")

-- Sample test cases for parseKeyValuePairs
testParseKeyValuePairs :: Test
testParseKeyValuePairs = TestCase $ do
  assertEqual
    "parseKeyValuePairs should parse valid key-value pairs"
    ([("key", JsonString "value")], "")
    (parseKeyValuePairs "\"key\": \"value\"}")
  assertEqual
    "parseKeyValuePairs should parse multiple key-value pairs"
    ([("key1", JsonString "value1"), ("key2", JsonString "value2")], "")
    (parseKeyValuePairs "\"key1\": \"value1\", \"key2\": \"value2\"}")
  assertEqual
    "parseKeyValuePairs should return empty list for empty object"
    ([], "")
    (parseKeyValuePairs "}")
  assertEqual
    "parseKeyValuePairs should handle excess white spaces"
    ([("key", JsonString "value with space")], "")
    (parseKeyValuePairs " \"key\" : \"value with space\" }")

-- Sample test cases for parseKeyValuePair
testParseKeyValuePair :: Test
testParseKeyValuePair = TestCase $ do
  assertEqual
    "parseKeyValuePair should parse a valid key-value pair"
    (Just (("key", JsonString "value"), ""))
    (parseKeyValuePair "\"key\": \"value\"")
  assertEqual
    "parseKeyValuePair should return Nothing for invalid key-value pair"
    Nothing
    (parseKeyValuePair "\"key\": value")
  assertEqual
    "parseKeyValuePair should handle excess white spaces"
    (Just (("key", JsonString "value"), ""))
    (parseKeyValuePair " \"key\" : \"value\" ")

-- Group all tests together
tests :: Test
tests =
  TestList
    [ testParseJsonString,
      testParseJsonNumber,
      testParseJsonBoolOrNull,
      testParseJsonObject,
      testParseJsonArray,
      testParseJsonValue,
      testParseKeyValuePairs,
      testParseKeyValuePair
    ]