{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1 solution

first :: (a -> b) -> (a, c) -> (b, c)
first f (x, y) = (f x, y)

instance Functor Parser where
  fmap f (Parser g) = Parser (\s -> case (g s) of
                                      (Just res) -> Just (first f res)
                                      Nothing    -> Nothing)
									  
-- Exercise 2 solution

instance Applicative Parser where
  pure x = Parser (\s -> Just (x, s))
  (<*>) (Parser f) (Parser g) 
        = Parser (\s -> case (f s) of
                          (Just (h, res)) -> fmap (first h) $ g res
                          Nothing         -> Nothing)
						  
-- Exercise 3 solution

abParser :: Parser (Char, Char)
abParser = (\c -> \x -> (c, x)) <$> (char 'a') <*> (char 'b') 

abParser_ :: Parser ()
abParser_ = (\c -> \x -> ()) <$> (char 'a') <*> (char 'b')

-- Idea is that you create one parser (A) which parses an int that begins with a space,
-- and a parser that parses just a positive integer (B). You want to apply
-- B first, generating a function that takes in A's result and makes a list
-- out of it, and then apply that on whatever B generates.
intPair :: Parser [Integer]
intPair = (\y -> \x -> [y, x]) <$> (posInt) <*> spIntParser
 where
  spIntParser = (\b -> \x -> x) <$> (char ' ') <*> (posInt)
  
-- Exercise 4 solution

instance Alternative Parser where
  empty = Parser (\s -> Nothing)
  (<|>) (Parser f1) (Parser f2) = Parser (\s -> f1 s <|> f2 s)
  
-- Exercise 5 solution

intOrUppercase :: Parser ()
intOrUppercase = (toUnit <$> posInt) <|> (toUnit <$> (satisfy isUpper))
 where
  toUnit x = ()