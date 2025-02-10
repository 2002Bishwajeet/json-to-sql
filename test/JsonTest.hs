module JsonTest (jsonTestList) where

import Json
import Test.HUnit

testJsonValueToString :: Test
testJsonValueToString =
  TestList
    [ "test JsonNull" ~: jsonValueToString JsonNull ~?= "NULL",
      "test JsonBool True" ~: jsonValueToString (JsonBool True) ~?= "TRUE",
      "test JsonBool False" ~: jsonValueToString (JsonBool False) ~?= "FALSE",
      "test JsonInt" ~: jsonValueToString (JsonInt 42) ~?= "42",
      "test JsonDouble" ~: jsonValueToString (JsonDouble 3.14) ~?= "3.14",
      "test JsonString" ~: jsonValueToString (JsonString "hello") ~?= "'hello'",
      "test JsonString with single quote" ~: jsonValueToString (JsonString "it's") ~?= "'it''s'",
      "test JsonArray" ~: jsonValueToString (JsonArray [JsonInt 1, JsonBool False]) ~?= "'[1 FALSE]'",
      "test JsonObject" ~: jsonValueToString (JsonObject [("key", JsonString "value")]) ~?= "'{'key': 'value'}'"
    ]

testJsonValueToSqlType :: Test
testJsonValueToSqlType =
  TestList
    [ "test JsonNull" ~: jsonValueToSqlType JsonNull ~?= "NULL",
      "test JsonBool" ~: jsonValueToSqlType (JsonBool True) ~?= "BOOLEAN",
      "test JsonInt" ~: jsonValueToSqlType (JsonInt 42) ~?= "INTEGER",
      "test JsonDouble" ~: jsonValueToSqlType (JsonDouble 3.14) ~?= "REAL",
      "test JsonString" ~: jsonValueToSqlType (JsonString "hello") ~?= "TEXT",
      "test JsonArray" ~: jsonValueToSqlType (JsonArray [JsonInt 1, JsonBool False]) ~?= "TEXT",
      "test JsonObject" ~: jsonValueToSqlType (JsonObject [("key", JsonString "value")]) ~?= "TEXT"
    ]

testIsJsonObject :: Test
testIsJsonObject =
  TestList
    [ "test JsonObject" ~: isJsonObject (JsonObject [("key", JsonString "value")]) ~?= True,
      "test JsonArray" ~: isJsonObject (JsonArray [JsonInt 1, JsonBool False]) ~?= False,
      "test JsonString" ~: isJsonObject (JsonString "hello") ~?= False
    ]

testIsOnlyJsonArrayObject :: Test
testIsOnlyJsonArrayObject =
  TestList
    [ "test JsonArray of JsonObjects" ~: isOnlyJsonArrayObject (JsonArray [JsonObject [("key", JsonString "value")]]) ~?= True,
      "test JsonArray of mixed values" ~: isOnlyJsonArrayObject (JsonArray [JsonInt 1, JsonObject [("key", JsonString "value")]]) ~?= False,
      "test JsonObject with JsonArray of JsonObjects" ~: isOnlyJsonArrayObject (JsonObject [("data", JsonArray [JsonObject [("key", JsonString "value")]])]) ~?= True,
      "test JsonObject with JsonArray of mixed values" ~: isOnlyJsonArrayObject (JsonObject [("data", JsonArray [JsonInt 1, JsonObject [("key", JsonString "value")]])]) ~?= False,
      "test JsonObject with non-array value" ~: isOnlyJsonArrayObject (JsonObject [("data", JsonString "value")]) ~?= False
    ]

jsonTestList = TestList [testJsonValueToString, testJsonValueToSqlType, testIsJsonObject, testIsOnlyJsonArrayObject]