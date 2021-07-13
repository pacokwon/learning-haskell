{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Sized
import Scrabble
import Buffer
import Data.Function

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ elt) = [elt]
jlToList (Append _ l r) = jlToList l ++ jlToList r

(!!?) :: [a] -> Int -> Maybe a
[]          !!? _ = Nothing
_           !!? i | i < 0 = Nothing
(x : xs)    !!? 0 = Just x
(x : xs)    !!? i = xs !!? (i - 1)

indexJ :: (Sized b, Monoid b) =>
    Int -> JoinList b a -> Maybe a

indexJ i jl = jlToList jl !!? i

getSizeTag :: (Sized b, Monoid b) => JoinList b a -> Int
getSizeTag = getSize . size . tag

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty           = Empty
dropJ i jl
  | i <= 0              = jl
  | i >= getSizeTag jl  = Empty
dropJ _ (Single _ _)    = Empty
dropJ i (Append m jll jlr)
  | i <= getSizeTag jll = dropJ i jll +++ jlr
  | otherwise           = dropJ (i - getSizeTag jll) jlr

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty           = Empty
takeJ i jl
  | i <= 0              = Empty
  | i >= getSizeTag jl  = jl
takeJ _ jl@(Single _ _) = jl
takeJ i (Append m jll jlr)
  | i <= getSizeTag jll = takeJ i jll
  | otherwise           = jll +++ takeJ (i - getSizeTag jll) jlr

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

instance Monoid m => Semigroup (JoinList m a) where
  (<>) = (+++)

instance Monoid m => Monoid (JoinList m a) where
  mempty = Empty

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString = mconcat . fmap strToJl . lines
    where
      strToJl s = Single (scoreString s, Size 1) s
  line = indexJ
  replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n + 1) jl
  numLines = getSizeTag
  value = getScore . fst . tag
