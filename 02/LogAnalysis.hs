{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log
import Data.Function

parseMessage :: String -> LogMessage
parseMessage str = let wordList = words str in
                       case words str of
                         ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
                         ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
                         ("E":lvl:ts:msg) -> LogMessage (Error (read lvl)) (read ts) (unwords msg)
                         _ -> Unknown (unwords wordList)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@LogMessage{} Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts1 _) (Node left msg2@(LogMessage _ ts2 _) right)
  | ts1 > ts2 = Node left msg2 (insert msg1 right)
  | otherwise = Node (insert msg1 left) msg2 right
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree lm rtree) = inOrder ltree ++ [lm] ++ inOrder rtree

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong xs = xs & filter (severeThan 50) & build & inOrder & extractMessage

severeThan :: Int -> LogMessage -> Bool
severeThan n (LogMessage (Error lvl) _ _) = lvl > n
severeThan _ _ = False

extractMessage :: [LogMessage] -> [String]
extractMessage (LogMessage _ _ msg : msgs) = msg : extractMessage msgs
extractMessage _ = []
