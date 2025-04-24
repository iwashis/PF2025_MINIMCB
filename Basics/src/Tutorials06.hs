module Tutorials06 where 

import Control.Monad.State (State(..), get, put,modify, evalState, evalStateT, StateT(..))
import Control.Monad.Reader
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import System.IO.Error
import System.IO
import Control.Exception
import Data.Monoid (Sum(..))
import Data.Maybe (fromMaybe)


--
-- 1. **Algorytm losowy z użyciem State**
--  Zaimplementuj prosty generator liczb pseudolosowych przy użyciu monady State. Zdefiniuj funkcję 
-- `randomInt :: Int -> Int -> State Int Int`, która generuje liczbę całkowitą z podanego zakresu [a,b], 
-- używając [liniowego generatora kongruencyjnego](https://en.wikipedia.org/wiki/Linear_congruential_generator). 
-- Następnie napisz funkcję `randomList :: Int -> Int -> Int -> State Int [Int]`, 
-- która generuje listę n losowych liczb z zakresu [a,b]. Użyj funkcji `evalState` do uruchomienia obliczeń z określonym ziarnem (seed).

type Random = State Int 

randomInt :: Int -> Int -> Random Int
randomInt x y = do 
  s <- get 
  let s' = ( a * s + c ) `mod` m 
  put s'
  pure $ x + s `mod` (y - x)
  where 
    a = 22695477 
    c = 1
    m = 2 ^ 31

exampleRandomInt :: Int
exampleRandomInt = evalState (randomInt 10 15) 152

randomList :: Int -> Int -> Int -> Random [Int]
randomList 0 _ _ = pure [] 
randomList n a b = do 
  i <- randomInt a b
  is <- randomList (n-1) a b
  pure (i:is)

exampleRandomList :: [Int]
exampleRandomList = evalState (randomList 12 10 100) 152


-- 2. **Drzewo binarne i etykietowanie z State**
--    Zdefiniuj typ drzewa binarnego `data Tree a = Empty | Node a (Tree a) (Tree a)`. 
--    Następnie zaimplementuj funkcję `labelTree :: Tree a -> State Int (Tree (a, Int))`, 
--    która etykietuje każdy węzeł drzewa unikalnym numerem, używając monady State do śledzenia licznika. 
--    Numeracja powinna być w porządku preorder. Napisz również funkcję `countNodes :: Tree a -> State (Sum Int) (Tree a)`, która liczy węzły w drzewie, używając monady State do akumulacji.
--

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show)

labelTree :: Tree a -> State Int (Tree (a, Int))
labelTree Empty = pure Empty 
labelTree (Node x left right) = do 
  label <- get 
  let incLabel = label + 1
  put incLabel
  l <- labelTree left 
  r <- labelTree right 
  pure (Node (x, label) l r)


exampleTree :: Tree String
exampleTree = Node "a" (Node "b" (Node "cc" Empty Empty) (Node "d" Empty Empty)) Empty

lt = evalState (labelTree exampleTree) 10

-- 3. **Interaktywna kalkulacja z użyciem IO**
--    Napisz program `calculator :: IO ()`, który wczytuje od użytkownika dwie liczby oraz operację (dodawanie, odejmowanie, mnożenie, dzielenie) i wypisuje wynik. 
--    Program powinien obsługiwać błędy (np. dzielenie przez zero) i pytać użytkownika, czy chce kontynuować obliczenia. 
--    Użyj funkcji `getLine`, `readLn` oraz `putStrLn` do interakcji z użytkownikiem.

divide :: Int -> Int -> Maybe Double 
divide m n = if n == 0 then Nothing else Just $ (fromIntegral m) / (fromIntegral n) 

caseMaybe :: Maybe a -> (a -> b) -> b -> b 
caseMaybe Nothing _ b = b 
caseMaybe (Just x) f _ = f x


calculator :: IO ()
calculator = do 
  putStrLn "Podaj dwie liczby całkowite"
  x <- readLn :: IO Int  
  y <- readLn :: IO Int 
  putStrLn "Podaj operacje: +, -, /"
  op <- getLine
  case op of 
    "+" -> print ( x + y)
    "-" -> print ( x - y) 
    "/" -> caseMaybe (divide x y) print (putStrLn "Nie dzielimy przez zero") 
    _   -> do 
      putStrLn "Podales zly symbol operacji"
      calculator

-- 4. **Transformator ReaderT do konfiguracji aplikacji**
--    Zdefiniuj typ `Config`, który zawiera parametry aplikacji (np. `verbose :: Bool`, `maxRetries :: Int`). Następnie zaimplementuj funkcję `processItem :: String -> ReaderT Config IO Bool`, która przetwarza element i raportuje wynik. Funkcja powinna sprawdzać wartość `verbose` w konfiguracji i wypisywać dodatkowe informacje, gdy jest ustawiona na `True`. Na koniec napisz funkcję `processItems :: [String] -> ReaderT Config IO [Bool]`, która przetwarza listę elementów i zwraca listę wyników.

--
-- ReaderT - dodaje środowisko tylko do odczytu
--    newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
--
data Config = Config {verbose :: Bool }


config1 = Config True
config2 = Config False

processItem :: String -> ReaderT Config IO Bool
processItem string = do
  config <- ask 
  if verbose config then lift $ print (string ++ " in verbose mode") else lift $ print string 
  pure $ even (length string)  

p = runReaderT (processItem "tomek")


processItems :: [String] -> ReaderT Config IO [Bool]
processItems [] = pure []
processItems (x:xs) = do 
  p <- processItem x 
  ps <- processItems xs 
  pure (p:ps)

p2 = runReaderT (processItems ["tomek", "anna", "jacek"])

--
-- 5. **Obsługa błędów z ExceptT**
--    Napisz funkcję `readFileWithExcept :: FilePath -> ExceptT String IO String`, która próbuje odczytać zawartość pliku i obsługuje potencjalne błędy używając transformatora ExceptT. Następnie zaimplementuj funkcję `processFiles :: [FilePath] -> ExceptT String IO [String]`, która przetwarza listę plików, kontynuując nawet jeśli niektóre pliki nie mogą zostać odczytane. Dodaj funkcję pomocniczą `logError :: String -> ExceptT String IO ()`, która zapisuje błędy do pliku logów.
--
-- 6. **Łączenie transformatorów StateT i IO**
--    Zaimplementuj prosty symulator bankomatu używając transformatora StateT. Zdefiniuj typ `BankState` zawierający saldo konta. Napisz funkcje:
--    * `withdraw :: Int -> StateT BankState IO Bool` - próbuje wypłacić określoną kwotę
--    * `deposit :: Int -> StateT BankState IO ()` - wpłaca określoną kwotę
--    * `checkBalance :: StateT BankState IO Int` - sprawdza aktualne saldo
--    * `atmSession :: StateT BankState IO ()` - przeprowadza interaktywną sesję z użytkownikiem

data BankState = BankState {balance :: Int}
  deriving Show

type BankApp a = StateT BankState IO a 

withdraw :: Int -> BankApp Bool 
withdraw n = do
  state <- get 
  let isPossible = balance state > n
  if isPossible then do
    put (BankState $ (balance state) - n)
    lift $ putStrLn "Opracja pobrania sie powiodla"
  else 
    lift $ putStrLn "Operacja pobrania sie NIE powiodla"
  pure isPossible

deposit :: Int -> BankApp ()
deposit  n = do 
   modify (\bankstate -> BankState {balance = balance bankstate + n})
   lift $ putStrLn "Depozyt sie udal"

initState = BankState 100

op :: BankApp () 
op = do 
  isPossible <- withdraw 10
  if isPossible then deposit 20 else deposit 10

example = runStateT op initState 

--    Każda operacja powinna wypisywać odpowiednie komunikaty na ekranie oraz aktualizować stan konta.
--
-- 7. **Implementacja stosu transformatorów**
--    Zdefiniuj typ `AppM a = ReaderT Config (StateT AppState (ExceptT AppError IO)) a`, gdzie:
--    * `Config` zawiera parametry konfiguracyjne (np. `maxAttempts :: Int`)
--    * `AppState` zawiera stan aplikacji (np. `counter :: Int`, `lastOperation :: String`)
--    * `AppError` to typ reprezentujący możliwe błędy (np. `NetworkError String`, `ValidationError String`)
--    
--    Następnie zaimplementuj funkcje pomocnicze:
--    * `getConfig :: AppM Config` - pobiera konfigurację
--    * `getState :: AppM AppState` - pobiera stan
--    * `modifyState :: (AppState -> AppState) -> AppM ()` - modyfikuje stan
--    * `throwAppError :: AppError -> AppM a` - zgłasza błąd
--    * `runApp :: Config -> AppState -> AppM a -> IO (Either AppError (a, AppState))` - uruchamia obliczenie
--    
--    Na koniec zaimplementuj przykładową funkcję biznesową `processTransaction :: Transaction -> AppM Result`, która korzysta z powyższych funkcji pomocniczych.
