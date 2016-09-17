{-# OPTIONS_GHC -XFlexibleInstances -fno-warn-missing-methods #-}
module Homework_6 where

import Data.List

-- Exercise 1 solution

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 solution

fibs2 :: [Integer]
fibs2 = fib2 []
 where
   fib2 :: [Integer] -> [Integer]
   fib2 [] = fib2 [0]
   fib2 [0] = fib2 [0, 1]
   fib2 (f1:f2:fs) = [f1] ++ fib2 (f2:((f1+f2):fs))
   
-- Exercise 3 solution

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x sxs) = x:(streamToList sxs)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList
  
-- Exercise 4 solution

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5 solution

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) sy = Cons x (interleaveStreams sy xs) 

ruler :: Stream Integer
ruler = ruler_gen 0
  where
    ruler_gen n = interleaveStreams (streamRepeat n) (ruler_gen (n+1))

-- Exercise 6 solution

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate sn = streamMap (\n -> (-n)) sn
  (+) a@(Cons a0 a') b@(Cons b0 b') = Cons (a0+b0) (a' + b')
  (*) a@(Cons a0 a') b@(Cons b0 b') 
		= Cons (a0*b0) ((streamMap (*a0) b') + a' * b)
  
instance Fractional (Stream Integer) where
  (/) a@(Cons a0 a') b@(Cons b0 b')
		= Cons (a0 `div` b0) (streamMap (`div` b0) (a'-(a/b)*b'))

fibs3 :: Stream Integer
fibs3 = x/(1-x-x^2)

-- Exercise 7 solution

data Matrix = Matrix Integer Integer Integer Integer
	deriving (Eq, Show)

instance Num (Matrix) where
  (*) (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22)
		= Matrix (a11*b11+a12*b21) (a11*b12+a12*b22)
				 (a21*b11+a22*b21) (a21*b12+a22*b22)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = case (f^n) of
           (Matrix f11 f12 f21 f22) -> f12
  where
    f = Matrix 1 1 1 0