{-# LANGUAGE LambdaCase #-}
module Parser where

import Control.Applicative hiding (many)
import Data.List
import Control.Monad.Trans
import Control.Monad.State (StateT (runStateT), runStateT)
import Control.Monad.Trans.State.Lazy (StateT(StateT))
import Data.Char

-- Monad for parsing: 
-- Parser i a === i -> [ (a , i) ]
-- czy da się wyrazić tą monadę uzywając monad transformers?
-- czym jest StateT s m a ?




-- Parser type - a monad transformer that combines state (String) with non-determinism ([])
-- It processes input string and returns possible results with remaining input
type Parser a = StateT String [] a

-- Runs a parser on a string input and returns a list of results with remaining input
runParser :: Parser a -> String -> [(a, String)]
runParser = undefined

-- A parser that always fails (returns empty list of results)
nothing :: Parser a
nothing = undefined

-- Basic parser that consumes the first character of the input string
item :: Parser Char
item = undefined

-- Creates a parser that succeeds only if the next character satisfies a given predicate
sat :: (Char -> Bool) -> Parser Char
sat = undefined

-- Parser that matches any digit character (0-9)
digit :: Parser Char 
digit = undefined

-- Parser that matches any non-zero digit (1-9)
positiveDigit :: Parser Char
positiveDigit = undefined

-- Parser that matches any alphabetic character
alpha :: Parser Char
alpha = undefined

-- Parser that matches either an alphabetic character or a digit
alphaDigit :: Parser Char
alphaDigit = undefined

-- Creates a parser that matches a specific character
char :: Char -> Parser Char
char = undefined

-- Parser that matches an open parenthesis '('
openBracket = undefined

-- Parser that matches a close parenthesis ')'
closeBracket = undefined

-- Parser that matches a minus sign '-'
minus = undefined

-- Parser that matches a space character ' '
space = undefined

-- Applies a parser zero or more times, collecting results in a list
many :: Parser a -> Parser [a]
many = undefined

-- Applies a parser one or more times, collecting results in a list
many1 :: Parser a -> Parser [a]
many1 = undefined

-- Converts a string representation of a number to an integer
evaluate :: String -> Int
evaluate = undefined

-- Parses a positive integer (starting with a non-zero digit)
positiveInt :: Parser Int
positiveInt = undefined

-- Parses a negative integer (a minus sign followed by a positive integer)
negativeInt = undefined

-- Parses a zero integer (one or more '0' characters)
zeroInt = undefined

-- Parses any integer (positive, zero, or negative)
int :: Parser Int
int = undefined

-- Parses a sequence of space characters
spaces :: Parser String
spaces = undefined

-- Parses a semicolon character ';'
end :: Parser Char
end = undefined

-- Data type representing arithmetic expressions
data Expr where
  -- Represents an integer constant
  Constant :: Int -> Expr
  -- Represents addition of two expressions
  Plus :: Expr -> Expr -> Expr
  -- Represents multiplication of two expressions
  Times :: Expr -> Expr -> Expr
  deriving Show

-- Evaluates an arithmetic expression to an integer
eval :: Expr -> Int
eval = undefined

-- Parses an integer expression (a constant)
exprInt :: Parser Expr
exprInt = undefined

-- Parses a general arithmetic expression (constant, addition, or multiplication)
expr = undefined
