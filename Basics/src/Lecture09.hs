module Lecture09 where
import Control.Monad.State
import Control.Applicative hiding (many)
import Data.Char

-- Nasz zabawkowy język definiuje wyrażenia na dwóch poziomach:
-- Wyrażenia algebraiczne mogą być liczbami całkowitymi, zmiennymi lub kombinacjami za pomocą operatorów.
-- Wyrażenia programu obejmują wyrażenia algebraiczne, przypisania i instrukcje drukowania.
data AlgExpr = Constant Int | Var String | Add AlgExpr AlgExpr | Mul AlgExpr AlgExpr 
  deriving Show
data Expr = Algebraic AlgExpr | Assignment String AlgExpr | Print AlgExpr
  deriving Show 
newtype Program = Program [Expr]
  deriving Show

-- Przykładowy program, który przypisuje 5 do x, oblicza wyrażenie z x, przypisuje wynik do y, a następnie drukuje y.
exampleProgram :: String 
exampleProgram =  "let x:= 5;let y:=(x*(x+(5*9)));print y;"

keywords = ["let"]
-- Parsowanie przekształca tekst w ustrukturyzowane dane. Programowanie funkcyjne oferuje bardziej eleganckie podejście
-- za pomocą kombinatorów parserów.
--
-- Parser można postrzegać jako funkcję, która konsumuje dane wejściowe i generuje zarówno wynik, jak i pozostałe dane wejściowe.
-- Aby obsłużyć niejednoznaczność i niepowodzenia, używamy listy możliwych parsowań zamiast tylko jednego wyniku.
-- Pusta lista reprezentuje niepowodzenie, podczas gdy wiele wyników reprezentuje niejednoznaczne parsowania.

-- Łączymy StateT do śledzenia danych wejściowych z monadą listy do obsługi wielu możliwości parsowania.
type Parser a = StateT String [] a

-- Koncepcyjny diagram sznurkowy dla Parser a:
--                            ┌─── a₁
--                            │
--                            ├─── String₁
--                            │
--                            ├─── a₂
--                            │
--    String ───→[ Parser ]───┼─── String₂
--                            │
--                            ├─── a₃
--                            │
--                            ├─── String₃
--                            │
--                            └─── ... (więcej możliwych wyników)
--
-- Ta funkcja uruchamia parser na ciągu wejściowym, zwracając wszystkie możliwe wyniki parsowania z pozostałymi danymi wejściowymi.
runParser :: Parser a -> String -> [(a, String)]
runParser = runStateT 

-- Parser 'nothing' zawsze kończy się niepowodzeniem, zwracając pustą listę niezależnie od danych wejściowych.
nothing :: Parser a 
nothing = StateT $ const [] 

-- Podstawowy parser 'satisfy' konsumuje znak, jeśli spełnia warunek predykatu.
-- Kończy się niepowodzeniem na pustym wejściu lub jeśli pierwszy znak nie przejdzie testu.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = StateT g 
  where 
    g "" = []
    g (x:xs) = [(x,xs) | f x] 

-- Parser 'char' rozpoznaje określony znak, używając równości jako predykatu.
char :: Char -> Parser Char
char c = satisfy (== c) 

-- Parser 'string' rozpoznaje ciąg znaków. Dla pustego celu natychmiast kończy się sukcesem.
-- Dla niepustych celów dopasowuje pierwszy znak, a następnie rekurencyjnie dopasowuje resztę.
string :: String -> Parser String
string "" = StateT $ \napis -> [("", napis)] -- to samo co pure ""
string (x:xs) = do 
  ch <- char x
  chrs <- string xs 
  pure (ch:chrs)
  
-- Parser 'many' stosuje inny parser wielokrotnie, aż do niepowodzenia, zbierając wszystkie wyniki.
-- Używa <|> do wyboru między: 1) parsowaniem co najmniej jednego elementu lub 2) zwróceniem pustej listy.
--
-- Operator wyboru alternatywnego: <|>
--
-- Operator <|> jest fundamentalny dla kombinatorów parserów, reprezentując wybór między strategiami parsowania.
-- Wywodzi się z wczesnych funkcyjnych bibliotek parserów i jest teraz częścią typeklasy Alternative w Haskellu,
-- która abstrahuje pojęcie wyboru w różnych kontekstach.
--
-- W naszym przypadku operator <|> jest zdefiniowany dla monady listowej a -> [a] poprzez operator konkatenacji.
-- Następnie, dla każdej monady m, dla której mamy zdefiniowane <|>, możemy łatwo wprowadzić definicję dla monady StateT s m
-- Proste cwiczenie: napisac explicite implementacje <|> dla monady Parser a.
many :: Parser a -> Parser [a] 
many parser = (do 
  x <- parser 
  xs <- many parser 
  pure (x:xs)) <|> pure []

-- Parser 'many1' wymaga co najmniej jednego udanego parsowania, a następnie zero lub więcej dodatkowych parsowań.
many1 :: Parser a -> Parser [a]
many1 parser = do 
  x <- parser 
  xs <- many parser 
  pure (x:xs)

-- Parser 'whitespace' konsumuje zero lub więcej spacji, umożliwiając elastyczne odstępy na wejściu.
whitespace :: Parser ()
whitespace = do 
  _ <- many (char ' ') 
  pure ()

-- Parser 'token' obsługuje białe znaki przed i po innym parserze, upraszczając składnię, gdzie
-- odstępy nie mają znaczenia.
token :: Parser a -> Parser a 
token parser = do
  _ <- whitespace 
  x <- parser 
  _ <- whitespace 
  pure x

-- Parser 'symbol' rozpoznaje określony ciąg z otaczającymi białymi znakami.
symbol :: String -> Parser String
symbol name = token (string name) 

-- Parser 'digit' rozpoznaje pojedynczy znak cyfry.
digit :: Parser Char
digit = satisfy isDigit 

-- Parser 'positiveInteger' konwertuje ciąg cyfr na wartość całkowitą.
positiveInteger :: Parser Int  
positiveInteger =  do 
  s <- many1 digit
  pure $ read s 

-- Parser 'integer' obsługuje zarówno liczby dodatnie, jak i ujemne, demonstrując kompozycję parserów.
integer :: Parser Int 
integer = positiveInteger <|> (do 
  _ <- char '-'
  _ <- whitespace
  int <- positiveInteger
  pure $ -int )


-- Parse an identifier (variable name)
identifier :: Parser String
identifier = do
  c <- satisfy isAsciiLower
  cs <- pure [] <|> identifier 
  let word = c:cs
  if word `elem` keywords then nothing 
  else pure word

fake :: Parser String
fake = do
  c <- satisfy f 
  cs <- pure [] <|> identifier 
  pure $ c:cs
  where 
    f x = isAsciiLower x || x =='(' || x == ')'


-- Parse parentheses
parens :: Parser a -> Parser a
parens parser = do 
  _ <- char '('
  x <- parser 
  _ <- char ')'
  pure x

-- 
-- data AlgExpr = Const Int | Var String | Add AlgExpr AlgExpr | Mul AlgExpr AlgExpr 
-- algExpr -234234 | 1234 | x | (algExpr + algExpr) | (Expr * Expr)
addAlgExpr :: Parser AlgExpr 
addAlgExpr = do 
  x <- parens p 
  pure x
  where 
    p = do 
      e1 <- algExpr 
      _ <- char '+'
      e2 <- algExpr
      pure $ Add e1 e2

mulAlgExpr :: Parser AlgExpr 
mulAlgExpr = do 
  x <- parens p 
  pure x
  where 
    p = do 
      e1 <- algExpr 
      _ <- char '*'
      e2 <- algExpr
      pure $ Mul e1 e2


algExpr :: Parser AlgExpr
algExpr = (Constant <$> integer) <|> (Var <$> identifier) <|> addAlgExpr <|> mulAlgExpr


-- Parse an assignment expression
assignExpr :: Parser Expr
assignExpr = do 
  _ <- symbol "let "
  var <- identifier
  _ <- symbol ":="
  e <- algExpr
  pure $ Assignment var e

-- Parse a print expression/
printExpr :: Parser Expr
printExpr = do 
  _ <- symbol "print "
  e <- algExpr
  pure $ Print e

-- Parse a semicolon-terminated expression
expr :: Parser Expr
expr = do 
  e <- assignExpr <|> printExpr <|> (Algebraic <$> algExpr)
  _ <- symbol ";" 
  pure e

-- Parse a list of expressions (zero or more)
expList :: Parser [Expr]
expList = many expr

-- Parse the end of file/input
eof :: Parser ()
eof = StateT $ \input -> if null input then [((), "")] else []
-- Parse a program (a list of expressions)
program :: Parser Program
program = do
  whitespace  -- Handle any leading whitespace
  es <- expList
  whitespace  -- Handle any trailing whitespace
  eof
  pure $ Program es

-- Main parsing function that returns either a parsed program or an error
parse :: String -> Either String Program
parse input = case runParser program input of
  [] -> Left "Parse error: could not parse program"
  [(prog, "")] -> Right prog  -- Successful parse with no remaining input
  [(_, rest)] -> Left $ "Parse error: unconsumed input: " ++ rest
  _ -> Left "Parse error: ambiguous parse results"

