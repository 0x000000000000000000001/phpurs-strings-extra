module Test.Dummy where

import Prelude

bsearch :: Int -> Int -> Int -> (Int -> Int -> Int) -> Int
bsearch a size array compare = go 0 size
  where
  go i k
    | i > k = 0
    | otherwise = go (compare a array) k

-- test
