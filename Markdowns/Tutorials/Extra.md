# Dodatkowe zadania

## Egzotyczne monady

**Monada Continuation i zarządzanie przepływem sterowania**  

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

   **Abort - przerywanie obliczeń**  
   Zaimplementuj funkcję `abort :: r -> Cont r a`, która natychmiast przerywa obliczenia i zwraca podany wynik.
   
   Napisz funkcję `factorialWithAbort :: Integer -> Cont Integer Integer`, która oblicza silnię liczby całkowitej, ale przerywa obliczenia z kodem błędu:
   * `-1` gdy argument jest ujemny
   * `-2` gdy argument jest większy niż 20

   **Exceptions - obsługa wyjątków**  
   Zaimplementuj prosty mechanizm obsługi wyjątków za pomocą funkcji:
   * `exceptions :: Cont (Either String a) a -> Either String a` - uruchamia obliczenia, które mogą zgłaszać wyjątki
   * `throwError :: String -> Cont (Either String a) b` - zgłasza wyjątek z komunikatem
   * `catchError :: Cont (Either String a) a -> (String -> Cont (Either String a) a) -> Cont (Either String a) a` - obsługuje wyjątki

   Stwórz prosty ewaluator wyrażeń arytmetycznych obsługujący operacje dodawania, mnożenia i dzielenia oraz zgłaszający wyjątek przy próbie dzielenia przez zero.

   **Backtracking - mechanizm nawrotów**  
   Zaimplementuj funkcję `backtracking :: [a] -> Cont ([a] -> [a]) a`, która pozwala na implementację algorytmów z nawrotami.

   Użyj mechanizmu backtrackingu do:
   * Znalezienia wszystkich trójek pitagorejskich o wartościach nieprzekraczających podanego ograniczenia 
   * Rozwiązania problemu [n-Hetmanów](https://pl.wikipedia.org/wiki/Problem_ośmiu_hetmanów) dla szachownicy o zadanym rozmiarze

   

**Monada Free i Domain Specific Languages**  

    Zdefiniuj:
    ```haskell
    data Free f a = Pure a | Free (f (Free f a))
    ```
    
    Zaimplementuj instancje `Functor`, `Applicative` i `Monad` dla `Free f`, gdzie `f` jest dowolnym funktorem.
    
    Następnie utwórz prosty język do modelowania transakcji finansowych:
    
    ```haskell
    type AccountID = Int 

    data FinancialF next = 
        Deposit AccountID Int next 
      | Withdraw AccountID Int next 
      | CheckBalance AccountID (Int -> next)
      | Transfer AccountID Int AccountID next
      | Fail String
      deriving Functor
    
    type Financial a = Free FinancialF a
    ```
    
    Zaimplementuj:
    
    - Funkcje pomocnicze: 
      ```haskell
      deposit :: AccountID -> Int -> Financial ()
      withdraw :: AccountID -> Int -> Financial ()
      checkBalance :: AccountID -> Financial Int
      transfer :: AccountID -> Int -> AccountID -> Financial ()
      failWith :: String -> Financial a
      ```
    
    - Interpreter dla tego języka, który wykonuje operacje na rzeczywistym stanie:
      ```haskell
      runFinancial :: Financial a -> StateT (M.Map AccountID Int) (Either String) a
      ```
    
    - Interpreter do testowania, który zamiast wykonywać operacje, zapisuje je jako logi:
      ```haskell
      traceFinancial :: Financial a -> Writer [String] (Either String a)
      ```
    
    - Zaimplementuj bardziej złożoną operację używającą monady Free:
      ```haskell
      transferBetweenAccounts :: AccountID -> Int -> AccountID -> Financial ()
      ```
      która transferuje środki między dwoma kontami, obsługując przy tym odpowiednio błędy 
      (niewystarczające środki, nieistniejące konto itp.)
