module Utils where

import Debug.Trace

(//) :: Int -> Int -> Int
a // b = truncate $ fromIntegral a / fromIntegral b

clamp :: (Num a, Ord a) => a -> a -> a -> a
clamp from to x
  | x < from = from
  | x > to = to
  | otherwise = x

debug = flip trace
