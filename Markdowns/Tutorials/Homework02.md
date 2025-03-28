# Zadania domowe z Haskella, część 2: Monada Reader

## Definicja

```haskell
newtype Reader r a = Reader {runReader :: r -> a}
-- ^ runReader wykonuje obliczenie Reader, dostarczając środowisko r i zwracając wynik a
```

Reader to monada reprezentująca obliczenia, które mogą odczytywać wartości ze współdzielonego środowiska.

## Podstawowe instancje

Zaimplementuj następujące instancje dla monady Reader:

```haskell
instance Functor (Reader r) where
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap = undefined  -- Przekształci wynik, zachowując dostęp do środowiska
```

```haskell
instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  pure = undefined  -- Tworzy Reader z wartością ignorującą środowisko
  
  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) = undefined  -- Aplikuje funkcję w kontekście Reader
```

```haskell
instance Monad (Reader r) where
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) = undefined  -- Sekwencjonuje operacje, przekazując środowisko
```

## Podstawowe operacje

Zaimplementuj podstawowe operacje monady Reader:

```haskell
-- Pobiera całe środowisko
ask :: Reader r r
ask = undefined

-- Stosuje funkcję do środowiska
asks :: (r -> a) -> Reader r a
asks = undefined

-- Modyfikuje środowisko dla określonego obliczenia
local :: (r -> r) -> Reader r a -> Reader r a
local = undefined
```

## Praktyczny przykład: System bankowy

Poniżej przedstawiono struktury danych do praktycznego zastosowania monady Reader w systemie bankowym.

```haskell
-- Konfiguracja aplikacji bankowej
data BankConfig = BankConfig 
  { interestRate :: Double    -- Stopa procentowa
  , transactionFee :: Int     -- Opłata za transakcję
  , minimumBalance :: Int     -- Minimalne wymagane saldo
  } deriving (Show)

-- Struktura danych konta
data Account = Account 
  { accountId :: String  -- Identyfikator konta
  , balance :: Int       -- Aktualny stan
  } deriving (Show)
```

Zaimplementuj następujące funkcje używające monady Reader:

```haskell
-- Oblicza odsetki dla konta na podstawie konfiguracji
calculateInterest :: Account -> Reader BankConfig Int
calculateInterest = undefined

-- Pobiera opłatę za transakcję z konta
applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee = undefined

-- Sprawdza czy konto spełnia wymóg minimalnego salda
checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance = undefined

-- Przetwarza konto wykonując kilka operacji z konfiguracją
processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount = undefined
