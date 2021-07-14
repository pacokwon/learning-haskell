module Party where

import Employee
import Data.List
import Data.Tree

instance Semigroup GuestList where
  GL emps1 f1 <> GL emps2 f2 = GL (emps1 <> emps2) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

instance Ord Employee where
  compare emp1 emp2 = compare (empName emp1) (empName emp2)

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (empFun emp + fun)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 > f2   = gl1
  | otherwise = gl2

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold func seed Node { rootLabel = label, subForest = trees } = func label children
  where children = map (treeFold func seed) trees

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (with, without)
  where
    with    = GL [boss] (empFun boss)
    without = GL [] 0

nextLevel boss gls = (with, without)
  where
    with    = foldr (mappend . snd) (GL [boss] (empFun boss)) gls
    without = mconcat (map fst gls)

maxFun :: Tree Employee -> GuestList
maxFun tree =
  let (with@(GL _ funWith), without@(GL _ funWithout)) = treeFold nextLevel (mempty, mempty) tree in
  case compare funWith funWithout of
    LT -> without
    EQ -> without
    GT -> with

format :: GuestList -> String
format (GL empls fun) =
  let fun = ["Total fun: " ++ show fun] in
  let names = map empName (sort empls) in
  unlines (fun ++ names)

main :: IO ()
main =
  (readLn :: IO (Tree Employee)) >>= putStr . format . maxFun
