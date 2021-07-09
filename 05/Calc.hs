{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add l r) = eval l + eval r
eval (ExprT.Mul l r) = eval l * eval r

evalStr :: String -> Maybe Integer
evalStr str = parseExp ExprT.Lit ExprT.Add ExprT.Mul str >>= (Just . eval)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a

instance Expr ExprT where
    lit = ExprT.Lit
    add = ExprT.Add
    mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
    lit = id
    add = (+)
    mul = (-)

instance Expr Bool where
    lit = (> 0)
    add = (||)
    mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
    lit = MinMax
    add (MinMax l) (MinMax r) = MinMax (max l r)
    mul (MinMax l) (MinMax r) = MinMax (min l r)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
    lit = Mod7
    add (Mod7 l) (Mod7 r) = Mod7 ((l + r) `mod` 7)
    mul (Mod7 l) (Mod7 r) = Mod7 ((l * r) `mod` 7)

instance Expr StackVM.Program where
    lit x = [StackVM.PushI x]
    add x y = x ++ y ++ [StackVM.Add]
    mul x y = x ++ y ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
