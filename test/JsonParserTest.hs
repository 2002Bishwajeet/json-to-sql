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

-- Group all tests together
tests :: Test
tests =
  TestList
    [ testParseJsonString,
      testParseJsonNumber,
      testParseJsonObject
    ]