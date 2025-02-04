module Utils (dropLeading, countDots, replace, sanitize) where

import Data.Char (isSpace)

dropLeading :: String -> String
dropLeading = dropWhile isSpace

-- Util function for counting dots in a string
countDots :: String -> Int
countDots = length . filter (== '.')

replace :: (Eq a) => a -> a -> [a] -> [a]
replace a b = map $ \c -> if c == a then b else c

sanitize :: String -> String
sanitize = replace ' ' '_'
