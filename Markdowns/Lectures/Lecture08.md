# Monady i Parsery

## Wprowadzenie

Parsowanie to proces analizy tekstu zgodnie z określoną gramatyką w celu przekształcenia go w strukturę danych. W programowaniu funkcyjnym parsery monadyczne stanowią eleganckie i modularne podejście do tworzenia parserów.

Wykorzystamy wiedzę o monadach z poprzednich wykładów do zbudowania biblioteki parserów, która pozwoli nam analizować wyrażenia algebraiczne w sposób deklaratywny i kompozycyjny.

## Monady a Parsowanie

Parser możemy zdefiniować jako funkcję, która przyjmuje ciąg znaków wejściowych i zwraca potencjalny wynik wraz z pozostałym, nieprzetworzonym tekstem. Jeśli parsowanie się nie powiedzie, parser może nie zwrócić żadnego wyniku.

### Definicja Parsera

Parsery można modelować jako monady, definiując typ parsera w następujący sposób:

```haskell
type Parser a = String -> [(a, String)]
```

Gdzie:
- `a` to typ wyniku parsowania
- wynikiem parsera jest lista par `(a, String)`, gdzie `a` to wynik parsowania, a `String` to nieprzetworzony tekst
- pusta lista oznacza, że parsowanie się nie powiodło
- lista z wieloma elementami reprezentuje wieloznaczne parsowanie (parsery niedeterministyczne)

Możemy również wykorzystać transformatory monad, implementując parser jako kombinację monady stanu (do śledzenia pozostałego tekstu) oraz monady listy (do obsługi niedeterminizmu):

```haskell
type Parser a = StateT String [] a
```

### Podstawowe Operacje Parsera

Zdefiniujmy kilka podstawowych operacji:

```haskell
-- Uruchamia parser na danym wejściu
runParser :: Parser a -> String -> [(a, String)]
runParser p input = runStateT p input

-- Parser, który zawsze zawodzi
nothing :: Parser a
nothing = StateT $ \_ -> []

-- Parser, który pobiera pierwszy znak z wejścia
item :: Parser Char
item = StateT $ \input -> case input of
  []     -> []  -- Pusty tekst, parsowanie się nie powiodło
  (c:cs) -> [(c, cs)]  -- Zwróć pierwszy znak i resztę tekstu
```

### Predykaty i Kombinatory

Teraz zdefiniujmy predykaty i kombinatory, które pozwolą nam tworzyć bardziej zaawansowane parsery:

```haskell
-- Parser, który przyjmuje znak spełniający dany predykat
sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then return c else nothing

-- Parser dla cyfry
digit :: Parser Char
digit = sat isDigit

-- Parser dla cyfry niezerowej
positiveDigit :: Parser Char
positiveDigit = sat (\c -> isDigit c && c /= '0')

-- Parser dla litery
alpha :: Parser Char
alpha = sat isAlpha

-- Parser dla litery lub cyfry
alphaDigit :: Parser Char
alphaDigit = sat isAlphaNum

-- Parser dla konkretnego znaku
char :: Char -> Parser Char
char c = sat (== c)

-- Parsery dla specyficznych znaków
openBracket = char '('
closeBracket = char ')'
minus = char '-'
space = char ' '
```

## Parsowanie Sekwencji i Alternatyw

Do budowania złożonych parserów potrzebujemy mechanizmów do parsowania sekwencji oraz alternatyw. Wykorzystamy do tego instancje Applicative i Alternative:

```haskell
-- Aplikuje parser zero lub więcej razy
many :: Parser a -> Parser [a]
many p = many1 p <|> return []

-- Aplikuje parser jeden lub więcej razy
many1 :: Parser a -> Parser [a]
many1 p = do
  x <- p
  xs <- many p
  return (x:xs)

-- Parsuje ciąg spacji
spaces :: Parser String
spaces = many space

-- Parsuje średnik oznaczający koniec wyrażenia
end :: Parser Char
end = char ';'
```

## Parsowanie Wyrażeń Liczbowych

Teraz zajmiemy się parsowaniem wyrażeń liczbowych:

```haskell
-- Konwersja ciągu znaków reprezentujących liczbę na wartość całkowitą
evaluate :: String -> Int
evaluate = read

-- Parser dla liczby dodatniej
positiveInt :: Parser Int
positiveInt = do
  d <- positiveDigit
  ds <- many digit
  return (evaluate (d:ds))

-- Parser dla liczby ujemnej
negativeInt :: Parser Int
negativeInt = do
  minus
  n <- positiveInt
  return (-n)

-- Parser dla zera
zeroInt :: Parser Int
zeroInt = do
  char '0'
  return 0

-- Parser dla dowolnej liczby całkowitej
int :: Parser Int
int = positiveInt <|> negativeInt <|> zeroInt
```

## Parsowanie Wyrażeń Algebraicznych

Zdefiniujmy typ danych dla wyrażeń algebraicznych:

```haskell
data Expr = Constant Int | Plus Expr Expr | Times Expr Expr 
  deriving Show
```

Teraz zaimplementujemy parsery dla wyrażeń algebraicznych:

```haskell
-- Ewaluacja wyrażenia algebraicznego
eval :: Expr -> Int
eval (Constant n) = n
eval (Plus e1 e2) = eval e1 + eval e2
eval (Times e1 e2) = eval e1 * eval e2

-- Parser dla wyrażenia będącego liczbą całkowitą
exprInt :: Parser Expr
exprInt = do
  n <- int
  return (Constant n)

-- Parser dla nawiasów
exprBrackets :: Parser Expr
exprBrackets = do
  openBracket
  e <- expr
  closeBracket
  return e

-- Parser dla czynnika (liczba lub wyrażenie w nawiasach)
factor :: Parser Expr
factor = exprInt <|> exprBrackets

-- Parser dla terminu (czynnik lub iloczyn czynników)
term :: Parser Expr
term = do
  f <- factor
  termRest f
  where
    termRest left = (do
      char '*'
      right <- factor
      termRest (Times left right))
      <|> return left

-- Parser dla wyrażenia (term lub suma termów)
expr :: Parser Expr
expr = do
  t <- term
  exprRest t
  where
    exprRest left = (do
      char '+'
      right <- term
      exprRest (Plus left right))
      <|> return left
```

## Przykład Użycia Parsera

Zobaczmy, jak możemy użyć naszego parsera do analizy wyrażeń algebraicznych:

```haskell
-- Przykład: Parsowanie i ewaluacja wyrażenia
parseAndEval :: String -> Maybe Int
parseAndEval input = case runParser expr input of
  [(e, "")] -> Just (eval e)  -- Poprawne parsowanie, cały tekst został przetworzony
  [(e, rest)] -> Nothing      -- Niepoprawne parsowanie, część tekstu nie została przetworzona
  [] -> Nothing               -- Parsowanie się nie powiodło
```

## Przykłady Parsowania z Obsługą Błędów

Przeanalizujmy kilka przykładów parsowania wyrażeń algebraicznych z odpowiednią obsługą błędów:

```haskell
-- Przykład 1: Poprawne wyrażenie
parseExample1 = runParser expr "2+3*4"
-- Wynik: [(Plus (Constant 2) (Times (Constant 3) (Constant 4)), "")]

-- Przykład 2: Niekompletne wyrażenie
parseExample2 = runParser expr "2+"
-- Wynik: [] (błąd: oczekiwano wyrażenia po '+')

-- Przykład 3: Niepoprawna składnia
parseExample3 = runParser expr "2+*3"
-- Wynik: [] (błąd: oczekiwano wyrażenia po '+', a napotkano '*')
```

## Zaawansowane Techniki Parsowania

W praktyce często używa się gotowych bibliotek do parsowania, takich jak `parsec` czy `megaparsec`, które oferują bardziej zaawansowane mechanizmy obsługi błędów i lepszą wydajność. Niemniej, zrozumienie fundamentalnych zasad parsowania monadycznego jest niezwykle cenne.
