module SqlConvertorTest (sqlConvertorTests) where

import Lib
import Test.HUnit

testCreateTable :: Test
testCreateTable = TestCase $ do
  let json = JsonObject [("id", JsonInt 1), ("name", JsonString "John Doe"), ("age", JsonInt 30)]
  let expected = "CREATE TABLE IF NOT EXISTS users(\nid INTEGER PRIMARY KEY,\nname TEXT,\nage INTEGER);"
  assertEqual "for (createTable \"users\" json)," expected (createTable "users" json)

testInsertTable :: Test
testInsertTable = TestCase $ do
  let json = JsonObject [("id", JsonInt 1), ("name", JsonString "John Doe"), ("age", JsonInt 30)]
  let expected = "INSERT INTO users VALUES (1, 'John Doe', 30);"
  assertEqual "for (insertTable \"users\" json)," expected (insertTable "users" json)

testCreateTableWithSpecialChars :: Test
testCreateTableWithSpecialChars = TestCase $ do
  let json = JsonObject [("id", JsonInt 1), ("name", JsonString "John Doe"), ("age", JsonInt 30)]
  let expected = "CREATE TABLE IF NOT EXISTS users_table(\nid INTEGER PRIMARY KEY,\nname TEXT,\nage INTEGER);"
  assertEqual "for (createTable \"users_table\" json)," expected (createTable "users_table" json)

testInsertTableWithSpecialChars :: Test
testInsertTableWithSpecialChars = TestCase $ do
  let json = JsonObject [("id", JsonInt 1), ("name", JsonString "John Doe"), ("age", JsonInt 30)]
  let expected = "INSERT INTO users_table VALUES (1, 'John Doe', 30);"
  assertEqual "for (insertTable \"users_table\" json)," expected (insertTable "users_table" json)

sqlConvertorTests :: Test
sqlConvertorTests = TestList [testCreateTable, testInsertTable, testCreateTableWithSpecialChars, testInsertTableWithSpecialChars]
