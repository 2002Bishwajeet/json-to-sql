module UtilTest (utilTests) where

import Test.HUnit
import Utils

-- Test case for dropLeading
testDropLeading :: Test
testDropLeading = TestCase $ do
  -- Test for string with leading spaces
  assertEqual
    "dropLeading should remove leading spaces"
    "Hello"
    (dropLeading "   Hello")

  -- Test for string without leading spaces
  assertEqual
    "dropLeading should not alter string with no leading spaces"
    "World"
    (dropLeading "World")

  -- Test for empty string
  assertEqual
    "dropLeading should return empty string if input is empty"
    ""
    (dropLeading "")

  -- Test for string with all spaces
  assertEqual
    "dropLeading should return empty string if input contains only spaces"
    ""
    (dropLeading "    ")

-- Test case for countDots
testCountDots :: Test
testCountDots = TestCase $ do
  -- Test for string with no dots
  assertEqual
    "countDots should return 0 when no dots are present"
    0
    (countDots "Hello")

  -- Test for string with one dot
  assertEqual
    "countDots should return 1 when one dot is present"
    1
    (countDots "Hello.")

  -- Test for string with multiple dots
  assertEqual
    "countDots should return the correct count of dots"
    5
    (countDots "Hello.world.this.is.it.")

  -- Test for string with dots at the beginning and end
  assertEqual
    "countDots should count dots at the beginning and end"
    3
    (countDots ".Hello.world.")

  -- Test for string with dots in between words
  assertEqual
    "countDots should count dots in between words"
    6
    (countDots "This.is.a.test.string.with.dots")

  -- Test for string with no characters (empty string)
  assertEqual
    "countDots should return 0 for empty string"
    0
    (countDots "")

-- Group all utility tests together
utilTests :: Test
utilTests = TestList [testDropLeading, testCountDots]
