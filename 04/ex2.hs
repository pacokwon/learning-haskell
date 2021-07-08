import Data.List

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldl' (flip insertT) Leaf

insertT :: a -> Tree a -> Tree a
insertT val Leaf = Node 0 Leaf val Leaf
insertT val (Node h lt m rt)
  | height lt < height rt = Node h (insertT val lt) m rt
  | height lt > height rt = Node h lt m (insertT val rt)
  | count lt < count rt = Node h (insertT val lt) m rt
  | count lt > count rt = Node h lt m (insertT val rt)
  | otherwise = Node h' lt m rt'
      where
        rt' = insertT val rt
        h' = 1 + height rt'

height :: Tree a -> Integer
height (Node h _ _ _) = h
height _ = -1

count :: Tree a -> Integer
count (Node _ lt _ rt) = 1 + count lt + count rt
count _ = 0
