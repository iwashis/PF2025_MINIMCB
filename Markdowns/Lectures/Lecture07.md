# Monada IO i Transformatory Monad

## Wprowadzenie

Monada IO jest fundamentalnym mechanizmem w Haskellu, który umożliwia wykonywanie operacji wejścia/wyjścia w czystym języku funkcyjnym. Z kolei transformatory monad pozwalają na łączenie różnych monad w celu uzyskania bardziej złożonych i użytecznych abstrakcji. W dzisiejszym wykładzie omówimy zarówno monadę IO, jak i transformatory monad, analizując ich definicje, właściwości oraz praktyczne zastosowania.

## Monada IO

Monada IO pozwala na modelowanie obliczeń, które wchodzą w interakcję z zewnętrznym światem (czytanie z plików, wypisywanie na ekran, komunikacja sieciowa itp.) w sposób zgodny z paradygmatem czystego programowania funkcyjnego.

### Potrzeba Monady IO

W czystym programowaniu funkcyjnym funkcje nie mogą mieć efektów ubocznych - muszą zawsze zwracać taki sam wynik dla tych samych argumentów. Jednak interakcja z rzeczywistym światem z definicji wprowadza efekty uboczne. Monada IO rozwiązuje ten problem, enkapsulując efekty uboczne w typie danych.

### Definicja Monady IO

IO można zrozumieć jako wartość, która reprezentuje obliczenie generujące wynik. Koncepcyjnie, można by zdefiniować IO w następujący sposób:

```haskell
newtype IO a = IO { runIO :: World -> (World, a) }
```

Gdzie:
- `a` to typ wyniku
- `World` reprezentuje stan świata zewnętrznego
- `runIO` to funkcja, która przyjmuje aktualny stan świata i zwraca parę: nowy stan świata oraz wynik typu `a`

Jednakże, rzeczywista implementacja IO w Haskellu jest ukryta przed programistą, aby zapewnić bezpieczeństwo i niezmienność. Faktyczny typ `World` nigdy nie jest dostępny bezpośrednio.


### Interpretacja Diagramowa

Podobnie jak w przypadku monady State, funkcje `f :: a -> IO b` wykorzystujące IO można interpretować diagramowo:

```
a   ───┐          ┌─── b
       │          │
       └──→[  f ]─┘
       │          │
World ─┘          └─── World
```


### Podstawowe Operacje IO

Oto kilka podstawowych operacji IO:

```haskell
-- Odczytuje linię ze standardowego wejścia
getLine :: IO String

-- Wypisuje tekst na standardowe wyjście
putStrLn :: String -> IO ()

-- Odczytuje zawartość pliku
readFile :: FilePath -> IO String

-- Zapisuje tekst do pliku
writeFile :: FilePath -> String -> IO ()
```

## Przykład 1: Prosty Program IO

Przykładowy program wykorzystujący monadę IO do interakcji z użytkownikiem:

```haskell
greet :: IO ()
greet = do
  putStrLn "Jak masz na imię?"
  name <- getLine
  putStrLn $ "Cześć, " ++ name ++ "!"

main :: IO ()
main = do
  greet
  putStrLn "Podaj dwie liczby do dodania:"
  a <- readLn :: IO Int
  b <- readLn :: IO Int
  let sum = a + b
  putStrLn $ "Suma: " ++ show sum
```

## Transformatory Monad

Transformatory monad pozwalają na łączenie funkcjonalności różnych monad. Dzięki temu możemy tworzyć bardziej złożone struktury monadyczne, które łączą w sobie właściwości wielu monad.

### Potrzeba Transformatorów Monad

Rozważmy sytuację, w której chcemy korzystać jednocześnie z funkcjonalności monady IO oraz monady State. Moglibyśmy próbować zagnieżdżać te monady:

```haskell
-- Próba połączenia IO i State
type Program s a = State s (IO a)
```

Jednakże, takie podejście rodzi problemy:
1. Trudności z łańcuchowaniem operacji (>>=)
2. Konieczność ręcznego "wypakowywania" zagnieżdżonych monad
3. Skomplikowany kod z wieloma zagnieżdżeniami

Transformatory monad rozwiązują te problemy.

### Definicja Transformatora Monady

Transformator monady to typ parametryczny, który bierze monadę jako parametr i tworzy nową monadę, łączącą funkcjonalności obu struktur. Na przykład, `StateT` to transformator, który dodaje funkcjonalność stanu do dowolnej monady:

```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }
```

Gdzie:
- `s` to typ stanu
- `m` to monada bazowa
- `a` to typ wyniku

### Przykłady Transformatorów Monad

Oto kilka popularnych transformatorów monad:

1. `StateT` - dodaje stan do dowolnej monady. Przypomnijmy, że:
```haskell
newtype StateT s m a = StateT { runStateT :: s -> m (s, a) }
```
2. `ReaderT` - dodaje środowisko tylko do odczytu
```haskell
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
```
3. `WriterT` - dodaje możliwość akumulowania wartości (np. logów)
```haskell
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
```
4. `ExceptT` - dodaje obsługę błędów
```haskell
newtype ExceptT e m a = ExceptT { runExceptT :: m (Either e a) }
```
5. `MaybeT` - dodaje możliwość braku wartości
```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

### Instancje dla StateT

Zobaczmy, jak zdefiniowane są instancje `Functor`, `Applicative` i `Monad` dla `StateT`:

```haskell
instance (Functor m) => Functor (StateT s m) where
  -- fmap :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT g) = StateT $ \s -> 
    fmap (\(s', a) -> (s', f a)) (g s)

instance (Monad m) => Applicative (StateT s m) where
  -- pure :: a -> StateT s m a
  pure a = StateT $ \s -> return (s, a)
  
  -- (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  StateT sf <*> StateT sa = StateT $ \s -> do
    (s', f) <- sf s
    (s'', a) <- sa s'
    return (s'', f a)

instance (Monad m) => Monad (StateT s m) where
  -- (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
  StateT sa >>= f = StateT $ \s -> do
    (s', a) <- sa s
    runStateT (f a) s'
```

### Podstawowe Operacje dla Transformatorów

Dla każdego transformatora monady, istnieją operacje umożliwiające interakcję z monadą bazową oraz dostęp do funkcjonalności transformatora:

```haskell
-- "Podnosi" obliczenie z monady bazowej do transformatora
lift :: (Monad m) => m a -> t m a

-- Przykładowo dla StateT:
lift :: (Monad m) => m a -> StateT s m a
lift m = StateT $ \s -> do
  a <- m
  return (s, a)
```

Operacje specyficzne dla `StateT`:

```haskell
-- Pobiera aktualny stan
get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)

-- Ustawia nowy stan
put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> return (s, ())

-- Modyfikuje stan za pomocą funkcji
modify :: Monad m => (s -> s) -> StateT s m ()
modify f = StateT $ \s -> return (f s, ())
```

## Przykład 2: Aplikacja z Użyciem Transformatorów Monad

Rozważmy przykład aplikacji, która łączy IO (dla operacji wejścia/wyjścia) oraz State (dla przechowywania stanu aplikacji).

```haskell
import Control.Monad.Trans
import Control.Monad.Trans.State

-- Definicja stanu aplikacji
data AppState = AppState {
  userName :: String,
  score :: Int,
  gameOver :: Bool
} deriving (Show)

-- Typ reprezentujący obliczenia w naszej aplikacji
type AppM a = StateT AppState IO a

-- Wyświetla aktualny stan
displayState :: AppM ()
displayState = do
  state <- get
  lift $ putStrLn $ "Użytkownik: " ++ userName state
  lift $ putStrLn $ "Wynik: " ++ show (score state)

-- Zwiększa wynik
increaseScore :: Int -> AppM ()
increaseScore points = modify $ \state ->
  state { score = score state + points }

-- Pyta użytkownika o imię i aktualizuje stan
askForName :: AppM ()
askForName = do
  lift $ putStrLn "Podaj swoje imię:"
  name <- lift getLine
  modify $ \state -> state { userName = name }

-- Inicjalizacja stanu i uruchomienie aplikacji
initialState :: AppState
initialState = AppState { userName = "", score = 0, gameOver = False }

-- Przykładowe użycie
runApp :: IO ()
runApp = do
  result <- runStateT (do
    askForName
    displayState
    increaseScore 10
    displayState
  ) initialState
  print $ snd result -- Wypisanie końcowego stanu
```

## Przykład 3: Łączenie Wielu Transformatorów

Możemy łączyć więcej niż dwa transformatory monad. Poniższy przykład pokazuje, jak stworzyć stos monadyczny, który łączy IO, State i Either (obsługa błędów):

```haskell
import Control.Monad.Trans
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

-- Definicja typów błędów
data AppError = InvalidInput String | GameError String deriving (Show)

-- Definicja stanu aplikacji
data GameState = GameState {
  playerHealth :: Int,
  playerScore :: Int
} deriving (Show)

-- Stos monadyczny łączący IO, State i Either
type GameM a = ExceptT AppError (StateT GameState IO) a

-- Funkcje pomocnicze
throwError :: AppError -> GameM a
throwError err = ExceptT $ return $ Left err

liftIO :: IO a -> GameM a
liftIO = lift . lift

getState :: GameM GameState
getState = lift get

modifyState :: (GameState -> GameState) -> GameM ()
modifyState = lift . modify

-- Przykład użycia
takeDamage :: Int -> GameM ()
takeDamage amount = do
  state <- getState
  let newHealth = playerHealth state - amount
  if newHealth <= 0
    then throwError $ GameError "Gracz stracił wszystkie punkty życia!"
    else modifyState $ \s -> s { playerHealth = newHealth }

addScore :: Int -> GameM ()
addScore points = modifyState $ \s -> s { playerScore = playerScore s + points }

-- Uruchomienie przykładu
runGame :: IO ()
runGame = do
  let initialState = GameState 100 0
  result <- runStateT (runExceptT $ do
    liftIO $ putStrLn "Rozpoczynamy grę!"
    addScore 10
    takeDamage 20
    state <- getState
    liftIO $ putStrLn $ "Stan gracza: " ++ show state
  ) initialState
  
  case result of
    (Left err, state) -> putStrLn $ "Błąd: " ++ show err
    (Right _, state) -> putStrLn $ "Stan końcowy: " ++ show state
```


