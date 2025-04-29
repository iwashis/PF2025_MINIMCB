module Lecture09 where

import Control.Monad.State
import Control.Monad (mzero)
import Control.Applicative hiding (many)
import Data.Char


-- an expression is: 
-- algexpr = integer | variable x | algexpr + algexpr | algexpr * algexpr
-- expr =    x := algebrexpr | algebr
data AlgExpr = Const Int | Var String | Add AlgExpr  AlgExpr | Mul AlgExpr AlgExpr 

data Expr = Algebraic AlgExpr | Assignment String AlgExpr

data Program = Program [Expr]

exampleProgram :: String 
exampleProgram =  "x := 5; y:= x * (x + (5*9));"

-- String -> Maybe Program 
-- String -> (String, a)
-- "4 * 50" ---> ("* 50", 4) ---> (" 50", *, pamietajac o 4) ->  ("", 50 (*, pamietam o 4)) -> Mul 50 4
-- String -> [(String, a)]
--
-- "40 * 4" -> []
-- Parser type using StateT with list as the underlying monad for non-determinism
type Parser a = StateT String [] a -- StateT (String -> [(String, a)])

runParser :: Parser a -> String -> [(a, String)]
runParser parser string = runStateT parser string

nothing :: Parser a 
nothing = StateT $ const [] 

-- Satisfy predicate
satisfy :: (Char -> Bool) -> Parser Char
satisfy  f = StateT g 
  where 
    -- g :: String -> [(a, String)]
    g "" = []
    g (x:xs) = [(x,xs) | f x] 

-- Parse a specific character
char :: Char -> Parser Char
char c = satisfy ( == c) 

-- Parse a string
string :: String -> Parser String
string "" =  StateT $ \napis -> [("", napis)] -- to samo co pure "" 
string (x:xs) = do 
  ch <- char x
  chrs <- string xs 
  pure (ch:chrs)
  
many :: Parser a -> Parser [a] 
many parser = (do 
  x <- parser 
  xs <- many parser 
  pure $ x:xs) <|> pure []

many1 :: Parser a -> Parser [a]
many1 parser = do 
  x <- parser 
  xs <- many parser 
  pure (x:xs)

p = do 
  xs <- many1 (satisfy isNumber)
  _  <- char '*'
  pure xs 
-- Parse whitespace
whitespace :: Parser ()
whitespace = do 
  _ <- many (char ' ') 
  pure ()

-- Parse a token (with surrounding whitespace)
token :: Parser a -> Parser a 
token parser = do
  _ <- whitespace 
  x <- parser 
  _ <- whitespace 
  pure x

-- Parse a specific token string
symbol :: String -> Parser String
symbol name = token (string name) 

-- Parse a digit
digit :: Parser Char
digit = satisfy isDigit 

-- Parse an integer
positiveInteger :: Parser Int  
positiveInteger =  do 
  string <- many1 digit
  pure $ read string

integer :: Parser Int 
integer = positiveInteger <|> (do 
  char '-'
  _ <- whitespace
  int <- positiveInteger
  pure $ -int )

-- Parse an identifier (variable name)
identifier :: Parser String
identifier = undefined

-- Parse parentheses
parens :: Parser a -> Parser a
parens = undefined

-- Parse an expression
expr :: Parser Expr
expr = undefined

-- Parse an assignment expression
assignExpr :: Parser Expr
assignExpr = undefined

-- Parse a print expression
printExpr :: Parser Expr
printExpr = undefined

-- Parse an addition expression
addExpr :: Parser Expr
addExpr = undefined

-- Parse a multiplication expression
mulExpr :: Parser Expr
mulExpr = undefined

-- Parse a factor expression (constant, variable, or parenthesized expression)
factorExpr :: Parser Expr
factorExpr = undefined

-- Parse a constant expression
constExpr :: Parser Expr
constExpr = undefined

-- Parse a variable expression
varExpr :: Parser Expr
varExpr = undefined

-- Parse a program (a list of expressions)
program :: Parser Program
program = undefined

-- Optional parser
option :: a -> Parser a -> Parser a
option = undefined

-- Run the parser on an input string
parseExpr :: String -> Maybe Expr
parseExpr = undefined

-- Run the program parser on an input string
parseProgram :: String -> Maybe Program
parseProgram = undefined
