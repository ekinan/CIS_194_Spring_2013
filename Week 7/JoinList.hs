{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where
import Data.Monoid
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
				  | Single m a
				  | Append m (JoinList m a) (JoinList m a)
	deriving (Eq, Show)
	
-- Exercise 1 solutions

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single mx _) = mx
tag (Append mx _ _) = mx
	
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty jl = jl
(+++) jl Empty = jl
(+++) jlOne jlTwo = Append (tag jlOne <> tag jlTwo) jlOne jlTwo

-- Exercise 2 solutions

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

getNextIndex :: Int -> Int -> Int
getNextIndex i sz
	| i < sz = i
	| otherwise = (i - sz)
	
getNextTree :: (Sized b, Monoid b) => 
					Int -> JoinList b a -> JoinList b a -> JoinList b a
getNextTree i jlOne jlTwo
	| i < leftSz = jlOne
	| otherwise = jlTwo
  where
    leftSz = getSize $ size $ tag jlOne

testJl = Append (Size 8) (
			Append (Size 4) (
				Append (Size 2) (Single (Size 1) 1) (Single (Size 1) 2)
			) (
				Append (Size 2) (Single (Size 1) 3) (Single (Size 1) 4)
			)
		) (
			Append (Size 4) (
				Append (Size 2) (Single (Size 1) 5) (Single (Size 1) 6)
			) (
				Append (Size 2) (Single (Size 1) 7) (Single (Size 1) 8)
			)
		)
	
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i (Single _ it)
    | i == 0 = Just it
    | otherwise = Nothing
indexJ i (Append sz jlOne jlTwo)
    | i >= (getSize $ size sz) = Nothing
    | otherwise = indexJ (getNextIndex i leftSz) (getNextTree i jlOne jlTwo)
  where
    leftSz = getSize $ size $ tag jlOne
	
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl 
	| n <= 0 = jl
dropJ _ (Single _ _) = Empty
dropJ n (Append sz jlOne jlTwo)
	| n >= (getSize $ size sz) = Empty
	| n <= leftSz = (dropJ n jlOne) +++ jlTwo
	| otherwise = dropJ (n - leftSz) jlTwo
  where
    leftSz = getSize $ size $ tag jlOne
	
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ n (Single mx it) 
	| n == 1 = Single mx it
	| otherwise = Empty
takeJ n tree@(Append sz jlOne jlTwo)
	| n >= (getSize $ size sz) = tree
	| n <= leftSz = takeJ n jlOne
	| otherwise = (takeJ leftSz jlOne) +++ (takeJ (n - leftSz) jlTwo)
  where
    leftSz = getSize $ size $ tag jlOne
	
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- Exercise 4 solutions
instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ jOne jTwo) = toString jOne ++ toString jTwo
  
  fromString = buildTree . lines
  
  line n b = indexJ n b
  
  replaceLine n l jl
	| n < 0 = jl
	| n >= (getSize $ size $ tag jl) = jl
  replaceLine _ l (Single _ _) = Single (scoreString l, 1) l
  replaceLine n l (Append _ jlOne jlTwo) 
	| n < leftSz = replaceLine n l jlOne +++ jlTwo
	| otherwise = jlOne +++ replaceLine (n - leftSz) l jlTwo
    where
	  leftSz = getSize $ size $ tag jlOne
	  

buildTree :: [String] -> JoinList (Score, Size) String
buildTree [] = Empty
buildTree [x] = Single (scoreString x, 1) x
buildTree (xs) = buildTree (take mid xs) +++ buildTree (drop mid xs)
  where
	mid = length xs `div` 2
