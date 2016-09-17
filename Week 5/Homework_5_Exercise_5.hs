{-# OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances #-}
module Homework_5_Exercise_5 where
import Parser
import StackVM

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a
  
-- Exercise 5 solution

instance Expr Program where
  lit n = [PushI n]
  add prog1 prog2 = prog1 ++ prog2 ++ [Add]
  mul prog1 prog2 = prog1 ++ prog2 ++ [Mul]  
  
compile :: String -> Maybe Program
compile str = parseExp lit add mul str
 
 