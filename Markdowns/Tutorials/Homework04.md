# Zadania Domowe z Haskella, część 4 - STM i GADTs

## 1. Implementacja banku z STM
Zdefiniuj typ konta bankowego i zaimplementuj podstawowe operacje używając Software Transactional Memory:
```haskell
data BankAccount = BankAccount 
  { accountId :: Int
  , balance :: TVar Double
  }
```
Zaimplementuj następujące funkcje:
- `createAccount :: Int -> Double -> IO BankAccount` - tworzy nowe konto z początkowym saldem
- `deposit :: BankAccount -> Double -> STM ()` - wpłaca pieniądze na konto
- `withdraw :: BankAccount -> Double -> STM Bool` - wypłaca pieniądze (zwraca False jeśli niewystarczające środki)
- `transfer :: BankAccount -> BankAccount -> Double -> STM Bool` - przelewa pieniądze między kontami
- `getBalance :: BankAccount -> STM Double` - pobiera aktualny stan konta
- `processTransactions :: [BankAccount -> STM ()] -> [BankAccount] -> IO ()` - wykonuje listę transakcji atomowo

## 2. Kolejka współbieżna z STM
Zaimplementuj thread-safe kolejkę używając STM:
```haskell
data STMQueue a = STMQueue (TVar [a]) 
```
Funkcje do zaimplementowania:
- `newSTMQueue :: STM (STMQueue a)` - tworzy pustą kolejkę
- `enqueue :: a -> STMQueue a -> STM ()` - dodaje element do kolejki
- `dequeue :: STMQueue a -> STM (Maybe a)` - usuwa element z kolejki
- `isEmpty :: STMQueue a -> STM Bool` - sprawdza czy kolejka jest pusta
- `size :: STMQueue a -> STM Int` - zwraca rozmiar kolejki
- `producer :: STMQueue Int -> [Int] -> IO ()` - funkcja producenta dodająca elementy
- `consumer :: STMQueue Int -> Int -> IO [Int]` - funkcja konsumenta pobierająca n elementów

## 3. GADT dla typowanych wyrażeń
Korzystając z przykładu poniżej i tutoriala dot. GADT dołączonego do tego dokumentu
zaimplementuj GADT dla prostego języka programowania który zapobiega błędom typów w czasie kompilacji.

#### Wymagania języka:
Twój język powinien obsługiwać:

Literały: liczby całkowite, wartości logiczne, stringi
Operacje arytmetyczne: +, -, * (tylko na liczbach)
Operacje logiczne: &&, ||, ! (tylko na boolach)
Porównania: ==, <, > (zwracają bool)
Instrukcja warunkowa: if-then-else
Konkatenacja stringów: ++

Przykłady wyrażeń które powinny być poprawne:
```haskell
5 + 3
True && False
if True then 42 else 17
"hello" ++ " world"
5 > 3
```
Zaimplementuj: 
GADT Expr t - reprezentujący wyrażenia z typem t
Funkcja eval :: Expr t -> t - ewaluująca wyrażenia

#### Przykład GADT 
```haskell
data Expr t where
  IntLit :: Int -> Expr Int
  BoolLit :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  If :: Expr Bool -> Expr a -> Expr a -> Expr a
```

## 4. STM-owy system cache'u
Zaimplementuj system cache'u z TTL (Time To Live) używając STM:
```haskell
data CacheEntry a = CacheEntry 
  { value :: a
  , expiryTime :: UTCTime
  }

data STMCache k v = STMCache (TVar (Map k (CacheEntry v)))
```
Funkcje do zaimplementowania:
- `newCache :: STM (STMCache k v)` - tworzy nowy cache
- `put :: Ord k => k -> v -> NominalDiffTime -> STMCache k v -> STM ()` - dodaje element z TTL
- `get :: Ord k => k -> STMCache k v -> STM (Maybe v)` - pobiera element jeśli nie wygasł
- `evictExpired :: Ord k => STMCache k v -> STM Int` - usuwa wygasłe elementy
- `size :: STMCache k v -> STM Int` - zwraca liczbę aktywnych elementów
- `cleanupDaemon :: Ord k => STMCache k v -> IO ()` - demon czyszczący cache co 60 sekund

## 5. Distributed Counter z STM
Zaimplementuj rozproszony licznik używając STM do synchronizacji:
```haskell
data DistributedCounter = DistributedCounter 
  { nodeId :: Int
  , localCount :: TVar Int
  , remoteNodes :: TVar [TVar Int]
  }
```
Funkcje do zaimplementowania:
- `createNode :: Int -> IO DistributedCounter` - tworzy węzeł licznika
- `connectNodes :: DistributedCounter -> DistributedCounter -> STM ()` - łączy dwa węzły
- `increment :: DistributedCounter -> STM ()` - zwiększa lokalny licznik
- `getGlobalCount :: DistributedCounter -> STM Int` - pobiera globalną sumę ze wszystkich węzłów
- `synchronize :: [DistributedCounter] -> IO ()` - synchronizuje wszystkie węzły
- `stressTest :: Int -> Int -> IO ()` - test wydajności z n wątkami wykonującymi m operacji

## Uwagi
- Wszystkie zadania z STM powinny być thread-safe i używać transakcji atomowych
- Pamiętaj o importowaniu `Control.Concurrent.STM` oraz `{-# LANGUAGE GADTs #-}`

## Wprowadzenie do GADTs (Generalized Algebraic Data Types)

### Czym są GADTs?
GADTs to rozszerzenie zwykłych typów algebraicznych w Haskellu, które pozwalają na precyzyjniejsze określenie typów konstruktorów. W przeciwieństwie do zwykłych ADT, konstruktory GADT mogą zwracać różne konkretne instancje typu ogólnego.

### Składnia i podstawy
Aby używać GADTs, potrzebujesz rozszerzenia językowego:
```haskell
{-# LANGUAGE GADTs #-}
```

Podstawowa składnia:
```haskell
data MyType a where
  Constructor1 :: Int -> MyType Int
  Constructor2 :: String -> MyType String
  Constructor3 :: a -> a -> MyType a
```

### Różnica między ADT a GADT
Drzewa BST w języku ADT i GADT. Najpierw w ADT:
```haskell 
data BST a = Leaf a | Empty | Node a (BST a) (BST a) 
```
Natomiast w języku typów GADT wygląda następująco:

```haskell 
data BST a where
    Empty :: BST a 
    Leaf :: a -> BST a 
    Node :: a -> BST a -> BST a -> BST a
```

**Zwykły ADT:**
Inny przykład, którego nie da dobrze modelować za pomocą ADT, a z łatwością można to zrobić w GADT.
```haskell
data Expr a = IntLit Int | BoolLit Bool | Add (Expr a) (Expr a)
-- Problem: Add (IntLit 5) (BoolLit True) jest poprawne typowo!
```

**GADT:**
```haskell
data Expr a where
  IntLit :: Int -> Expr Int
  BoolLit :: Bool -> Expr Bool
  Add :: Expr Int -> Expr Int -> Expr Int
  Eq :: Expr a -> Expr a -> Expr Bool  
  If :: Expr Bool -> Expr a -> Expr a -> Expr a 
-- Add (IntLit 5) (BoolLit True) nie skompiluje się!
```

### Cechy GADTs

1. **Bezpieczeństwo typów w czasie kompilacji**
   ```haskell
   eval :: Expr t -> t
   eval (IntLit n) = n
   eval (BoolLit b) = b
   eval (Add e1 e2) = eval e1 + eval e2
   -- Kompilator wie, że Add zawsze zwraca Int
   ```

2. **Phantom types z gwarancjami**
   ```haskell
   data Safe
   data Unsafe
   
   data Database s where
     UnsafeDB :: String -> Database Unsafe
     SafeDB :: String -> Database Safe
   
   query :: Database Safe -> String -> IO Result
   -- Można wywołać tylko na bezpiecznej bazie danych
   ```
### Wskazówki do implementacji

1. **Pattern matching** - kompilator wie więcej o typach:
   ```haskell
   eval :: Expr t -> t
   eval expr = case expr of
     IntLit n -> n      -- kompilator wie, że t ~ Int
     BoolLit b -> b     -- kompilator wie, że t ~ Bool
   ```

2. **Pomocnicze funkcje typu** - czasem potrzebne są dodatkowe ograniczenia:
   ```haskell
   data TypeRep t where
     IntRep :: TypeRep Int
     BoolRep :: TypeRep Bool
   
   defaultValue :: TypeRep t -> t
   defaultValue IntRep = 0
   defaultValue BoolRep = False
   ``` 
