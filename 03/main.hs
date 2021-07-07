module Golf where

import Data.Function
import Data.List (transpose)

skips :: [a] -> [[a]]
skips = init . scanr (:) []

localMaxima :: Ord a => [a] -> [a]
localMaxima (x : y : z : xs)
  | x < y && y > z = y : localMaxima xs
  | otherwise = localMaxima xs
localMaxima _ = []

stat :: [Integer] -> [Int]
stat xs = map (\x -> length . filter (== x) $ xs) [0 .. 9]

makeBar :: Int -> Int -> String
makeBar len cnt = replicate (len - cnt) ' ' ++ replicate cnt '*'

histogram :: [Integer] -> String
histogram xs = xs & stat & map (makeBar longest) & transpose & (++ footer) & unlines
    where
        longest = xs & stat & foldl max 0
        footer = ["==========", "0123456789"]
