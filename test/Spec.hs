module Main (main) where

import JsonParserTest (tests)
import JsonTest (jsonTestList)
import SqlConvertorTest (sqlConvertorTests)
import Test.HUnit
import UtilTest (utilTests)

main :: IO ()
main = do
  runTestTT tests >>= print
  runTestTT utilTests >>= print
  runTestTT sqlConvertorTests >>= print
  runTestTT jsonTestList >>= print