module Util where

roundAwayFromZero :: Int -> Int -> Int
roundAwayFromZero n x
  | x `mod` n == 0 = x
  | x < 0 = x - n - (x `mod` n)
  | otherwise = x + n - (x `mod` n)

is32BitRange :: Int -> Bool
is32BitRange x = x >= -2147483648 && x <= 2147483647