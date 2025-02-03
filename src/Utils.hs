module Utils (dropLeading, countDots) where

import Data.Char (isSpace)

dropLeading :: String -> String
dropLeading = dropWhile isSpace

-- Util function for counting dots in a string
countDots :: String -> Int
countDots = length . filter (== '.')