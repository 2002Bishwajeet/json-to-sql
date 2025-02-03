module Main (main) where

import Lib

main :: IO ()
main = do
  let test1 = parseJsonObject "{\"key\": \"value\"}"
  let test2 = parseJsonObject "{\"outer\": {\"inner\": \"value\"}}"
  let test3 = parseJsonArray "[1, 2, 3]"
  let test4 = parseJsonValue "[true,false,null]"

  print test1
  print test2
  print test3
  print test4
