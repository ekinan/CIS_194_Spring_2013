{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (char ' ')

ident :: Parser String
ident = (:) <$> (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
 
-- Creates a parser that filters leading and trailing spaces for
-- a given parser
filterSpaces :: Parser a -> Parser a
filterSpaces p = (spaces *> p) <* spaces

-- Parser for an individual atom in the S-Expression
atomParser :: Parser SExpr
atomParser = filterSpaces $ A <$> ((I <$> ident) <|> (N <$> posInt)) 

-- Parser for a combination of s expressions
combParser :: Parser SExpr
combParser 
  = filterSpaces $ Comb <$> (((char '(') *> (zeroOrMore sExprParser)) <* (char ')'))

-- Overall s expression parser
sExprParser :: Parser SExpr
sExprParser = atomParser <|> combParser