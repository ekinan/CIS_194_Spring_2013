{-# OPTIONS_GHC -XFlexibleInstances -XTypeSynonymInstances #-}
module Homework_5 where
import Parser
import qualified Data.Map as M

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

class HasVars a where
  var :: String -> a
  
data VarExprT = Lit Integer
			  | Add VarExprT VarExprT
			  | Mul VarExprT VarExprT
              | Var String
			  
instance Expr VarExprT where
  lit n = Lit n
  add x y = Add x y
  mul x y = Mul x y
  
instance HasVars VarExprT where
  var x = Var x

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var x = \map -> M.lookup x map
  
combineMaybes :: (Integer -> Integer -> Integer) 
                -> Maybe Integer 
				-> Maybe Integer 
				-> Maybe Integer
combineMaybes _ Nothing _ = Nothing
combineMaybes _ _ Nothing = Nothing
combineMaybes f (Just n) (Just m) = Just (f n m)
  
--  x and y are the first and second values, respectively, to lookup.

instance Expr (M.Map String Integer -> Maybe Integer) where  
  lit n = \map -> Just n
  add x y = \map -> combineMaybes (+) (x map) (y map)
  mul x y = \map -> combineMaybes (*) (x map) (y map)

withVars :: [(String, Integer)] 
		 -> (M.Map String Integer -> Maybe Integer) 
		 -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
  
  

		 
 