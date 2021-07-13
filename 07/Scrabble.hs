{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

instance Semigroup Score where
  Score m <> Score n = Score (m + n)

instance Monoid Score where
  mempty = Score 0

scoreMapping :: [(Char, Int)]
scoreMapping =
  [
    ('A', 1), ('E', 1), ('I', 1), ('L', 1), ('N', 1), ('O', 1), ('R', 1), ('S', 1), ('T', 1), ('U', 1),
    ('D', 2), ('G', 2),
    ('B', 3), ('C', 3), ('M', 3), ('P', 3),
    ('F', 4), ('H', 4), ('V', 4), ('W', 4), ('Y', 4),
    ('K', 5),
    ('J', 8), ('X', 8),
    ('Q', 10), ('Z', 10)
  ]

score :: Char -> Score
score c = case lookup (toUpper c) scoreMapping of
            Just n  -> Score n
            Nothing -> Score 0

scoreString :: String -> Score
scoreString str = mconcat (map score str)

getScore :: Score -> Int
getScore (Score n) = n
