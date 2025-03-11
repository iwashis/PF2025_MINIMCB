# Monady i ich zastosowania

1. **Podstawy Monady Maybe**  

   Napisz funkcję `safeDivide :: Int -> Int -> Maybe Int`, która wykonuje bezpieczne dzielenie dwóch liczb całkowitych. 
   Jeżeli dzielnik jest równy zero, funkcja powinna zwrócić `Nothing`, w przeciwnym razie `Just` z wynikiem dzielenia.
   Następnie zaimplementuj funkcję `chainedDivision :: [Int] -> Maybe Int`, która przeprowadza sekwencję bezpiecznych 
   dzieleń, zaczynając od pierwszej liczby i dzieląc ją przez każdą kolejną. Użyj operatora `>>=` (bind).

2. **List Monada i eksploracja ścieżek**  

   Napisz funkcję `knights :: (Int, Int) -> [(Int, Int)]`, która dla danej pozycji skoczka na szachownicy 
   zwraca listę wszystkich możliwych ruchów skoczka. Następnie zaimplementuj funkcję `knightPaths :: Int -> (Int, Int) -> (Int, Int) -> [[(Int, Int)]]`, 
   która znajduje wszystkie możliwe ścieżki o długości `n` ruchów z jednej pozycji do drugiej. Wykorzystaj monadę list oraz 
   operator `>>=` do eksploracji wszystkich możliwych ścieżek.

3. **Monada State do śledzenia stanu**  

   Zdefiniuj funkcję `runningSum :: [Int] -> [Int]`, która dla listy liczb całkowitych zwraca listę sum bieżących.
   Przykładowo, dla listy `[1, 2, 3, 4]` wynikiem powinno być `[1, 3, 6, 10]`. Zaimplementuj tę funkcję 
   używając monady State, korzystając z operacji `get`, `put` i funkcji `runState` lub `evalState`.

4. **Implementacja własnej monady**  

   Zaimplementuj własną monadę `Logger a`, która będzie przechowywać wartość typu `a` wraz z logiem 
   operacji (listą napisów). Zdefiniuj instancje `Functor`, `Applicative` i `Monad` dla tego typu.
   Napisz funkcje pomocnicze:
   - `logMessage :: String -> Logger ()`
   - `runLogger :: Logger a -> (a, [String])`
   Następnie użyj tej monady do zaimplementowania funkcji `factorial :: Int -> Logger Int`, 
   która oblicza silnię i loguje każdy krok obliczeń.

5. **Fish operator (>=>) w praktyce**  

   Zaimplementuj funkcje `safeTail :: [a] -> Maybe [a]` i `safeHead :: [a] -> Maybe a`, 
   które bezpiecznie zwracają ogon i głowę listy, obsługując przypadek pustej listy. 
   Następnie użyj operatora kleisli composition (`>=>`) do stworzenia funkcji `safeSecond :: [a] -> Maybe a`, 
   która bezpiecznie zwraca drugi element listy.

6. **Monada Writer do akumulacji wyników**  
    
   Zaimplementuj funkcję `countNodes :: Tree a -> Writer Sum Int`, która liczy węzły w drzewie binarnym, 
   używając monady Writer do akumulacji sumy. Typ drzewa zdefiniuj jako 
   `data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)`. 

## Egzotyczne monady - dodatkowe zadania

7. **Monada Continuation i zarządzanie przepływem sterowania**  

    Zaimplementuj monadę `Cont r a`, gdzie:
    ```haskell
    newtype Cont r a = Cont ((a -> r) -> r)
    ```
    
    Zdefiniuj instancje `Functor`, `Applicative` i `Monad` dla tego typu. Następnie zaimplementuj funkcje:
    
    - `callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a` (call-with-current-continuation), 
      która pozwala na przechowywanie kontynuacji w zmiennej i wykorzystywanie jej do implementacji 
      zaawansowanych struktur sterowania.
    
    - `runCont :: Cont r a -> (a -> r) -> r`, która uruchamia kontynuację z podaną funkcją końcową.
    
    Użyj tej monady do zaimplementowania:
    
    - `abort :: Cont r a -> r` - przerywanie obliczeń i natychmiastowe zwracanie wyniku
    - `exceptions :: Cont (Either String a) a -> Either String a` - prostego mechanizmu obsługi wyjątków
    - `backtracking :: [a] -> Cont ([a] -> [a]) a` - mechanizmu do implementacji nawrotów (backtracking)
      w algorytmach przeszukiwania
    

8. **Monada Free i Domain Specific Languages**  

    Zdefiniuj:
    ```haskell
    data Free f a = Pure a | Free (f (Free f a))
    ```
    
    Zaimplementuj instancje `Functor`, `Applicative` i `Monad` dla `Free f`, gdzie `f` jest dowolnym funktorem.
    
    Następnie utwórz prosty język do modelowania transakcji finansowych:
    
    ```haskell
    data FinancialF next = 
        Deposit Int next 
      | Withdraw Int next 
      | CheckBalance (Int -> next)
      | Transfer Int String next
      | Fail String
      deriving Functor
    
    type Financial a = Free FinancialF a
    ```
    
    Zaimplementuj:
    
    - Funkcje pomocnicze: 
      ```haskell
      deposit :: Int -> Financial ()
      withdraw :: Int -> Financial ()
      checkBalance :: Financial Int
      transfer :: Int -> String -> Financial ()
      failWith :: String -> Financial a
      ```
    
    - Interpreter dla tego języka, który wykonuje operacje na rzeczywistym stanie:
      ```haskell
      runFinancial :: Financial a -> StateT (M.Map String Int) (Either String) a
      ```
    
    - Interpreter do testowania, który zamiast wykonywać operacje, zapisuje je jako logi:
      ```haskell
      traceFinancial :: Financial a -> Writer [String] (Either String a)
      ```
    
    - Zaimplementuj bardziej złożoną operację używającą monady Free:
      ```haskell
      transferBetweenAccounts :: Int -> String -> String -> Financial ()
      ```
      która transferuje środki między dwoma kontami, obsługując przy tym odpowiednio błędy 
      (niewystarczające środki, nieistniejące konto itp.)
