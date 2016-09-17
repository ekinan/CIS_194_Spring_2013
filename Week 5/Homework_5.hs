{-# OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances #-}
module Homework_5 where
import ExprT
import Parser
import qualified Data.Map as M

-- Exercise 1 solution

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Exercise 2 solution

evalStr :: String -> Maybe Integer
evalStr str = case (maybeExpr) of
				Nothing -> Nothing
				Just expr -> Just (eval expr)
 where
   maybeExpr = parseExp Lit Add Mul str
  
-- Exercise 3 solution

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a
  
instance Expr ExprT where
 lit x = Lit x
 add exprA exprB = Add exprA exprB
 mul exprA exprB = Mul exprA exprB
 
-- Exercise 4 solution

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Integer where
 lit n = n
 add x y = x + y
 mul x y = x * y
 
instance Expr Bool where
  lit n = n > 0
  add x y = x || y
  mul x y = x && y
  
instance Expr MinMax where
  lit n = MinMax n
  add (MinMax x) (MinMax y) = MinMax (max x y)
  mul (MinMax x) (MinMax y) = MinMax (min x y)
 
instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 x) (Mod7 y) = Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)

-- Code provided by HW:
testExpr :: Expr a => Maybe a
testExpr = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExpr :: Maybe Integer
testBool = testExpr :: Maybe Bool
testMM = testExpr :: Maybe MinMax
testSat = testExpr :: Maybe Mod7


  
  

		 
 