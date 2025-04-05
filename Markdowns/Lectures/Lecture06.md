# Wykład 7 - Monada State i Jej Zastosowania

## Wprowadzenie

Monada State jest fundamentalnym narzędziem umożliwiającym zarządzanie stanem w programowaniu funkcyjnym bez wprowadzania efektów ubocznych. 
W dzisiejszym wykładzie najpierw przypomnimy podstawowe koncepcje związane z monadą State, 
a następnie rozszerzymy naszą wiedzę o dodatkowe funkcjonalności i przeanalizujemy kilka praktycznych przykładów jej zastosowania.

## Monada State

Monada State pozwala na modelowanie obliczeń, które mają dostęp do pewnego stanu, mogą go modyfikować i przekazywać dalej przez obliczenie.

### Definicja Parametrycznego Typu `State s a`

Definicja typu parametrycznego `State` jest następująca:

```haskell
newtype State s a = State { runState :: s -> (s, a) }
```

Gdzie:
- `s` to typ stanu
- `a` to typ wyniku
- `runState` to funkcja, która przyjmuje stan początkowy typu `s` i zwraca parę: nowy stan typu `s` oraz wynik typu `a`

### Instancje Functor, Applicative i Monad

Typ parametryczny State posiada instancje dla klas `Functor`, `Applicative` i `Monad`:

```haskell
instance Functor (State s) where 
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f state = State $ (\(s, x) -> (s, f x)) . runState state 
    -- Działanie: uruchamiamy state, dostajemy parę (stan, wynik),
    -- zachowujemy stan i aplikujemy funkcję f do wyniku

instance Applicative (State s) where 
  -- pure :: a -> State s a 
  -- Tworzy obliczenie stanowe, które nie zmienia stanu i zwraca wartość x
  pure x = State (,x) -- funkcja (,x) to to samo co \s -> (s, x)
  
  -- Implementacja liftA2 dla State
  liftA2 f (State f1) (State f2) = State g -- g :: s -> (s, c) 
    where 
      g s = let 
              (s', x) = f1 s     -- Uruchamiamy pierwsze obliczenie, dostajemy nowy stan s' i wynik x
              (s'', y) = f2 s'   -- Uruchamiamy drugie obliczenie z nowym stanem, dostajemy s'' i y
              z = f x y          -- Łączymy wyniki obliczeń za pomocą funkcji f
            in (s'', z)          -- :: (s, c)
          
instance Monad (State s) where 
  -- state :: State s a, runState state :: s -> (s,a) 
  -- f :: a -> State s b
  state >>= f = State $ (\(st, x) -> runState (f x) st) . runState state
```

### Interpretacja Diagramowa

Dla dowolnych typów `a, b, s` funkcję `f :: a -> State s b` możemy na diagramie interpretować w następujący sposób:
```
a ───┐                 ┌─── b
     │                 │
     └──→[      f    ]─┘
     │                 │
s ───┘                 └─── s
```
Powyższa interpretacja jest konsekwencją następującego ciągu wzajemnych jednocznacznych odpowiedniości:

```
  f :: a -> State s b 
────────────────────────
f' :: a -> (s -> (s,b))
────────────────────────
 f'' :: a -> s -> (s,b)
────────────────────────
f''' :: (a,s) -> (s,b)
```



Kompozycja funkcji monadycznych (`>=>`) wygląda następująco:

```
f1 :: a -> State s b 
f2 :: b -> State s c 
f3 :: c -> State s d

f1 >=> f2 >=> f3:

a ───┐                 ┌─── b      b ───┐                 ┌─── c      c ───┐                 ┌─── d
     │                 │                │                 │                │                 │
     └──→[     f1    ]─┘                └──→[     f2    ]─┘                └──→[     f3    ]─┘
     │                 │                │                 │                │                 │
s ───┘                 └─── s      s ───┘                 └─── s      s ───┘                 └─── s
```
Oznacza to, że po zastosowaniu jednocznacznej odpowiedniości `f <=> f'''` przedstawionej powyżej operator `>=>`
staje się zwykłym składaniem strzałek typu `(s,x) -> (s, y)`.

## Podstawowe Operacje State

Teraz zdefiniujemy podstawowe operacje dla monady State, które są niezwykle przydatne w praktycznych zastosowaniach:

### Funkcja get

Funkcja `get` pozwala na odczytanie aktualnego stanu:

```haskell
get :: State s s 
get = State $ \s -> (s, s)
```

Operacja `get` nie modyfikuje stanu, a jedynie zwraca go jako wynik obliczenia:

```
                       ┌─── s
                       │
      ──→[    get    ]─┘
     │                 │
s ───┘                 └─── s
```

Przykład użycia:
```
runState get 5 = (5, 5)
```

### Funkcja put

Funkcja `put` pozwala na ustawienie nowego stanu:

```haskell
put :: s -> State s ()
put s = State $ \_ -> (s, ())
```

Operacja `put` całkowicie zastępuje aktualny stan nową wartością i zwraca jednostkowy wynik `()`:

```
s1 :: s  ──┐                 ┌─── ()
           │                 │
           └──→[    put    ]─┘
           │                 │
s2 :: s ───┘                 └─── s1 :: s
```

Przykład użycia:
```
> runState (put 10) 5 = (10, ())
```

### Funkcja modify

Funkcja `modify` pozwala na modyfikację aktualnego stanu za pomocą funkcji:

```haskell
modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())
```

Alternatywnie, `modify` można zdefiniować w kategoriach `get` i `put`:

```haskell
modify f = do
  s <- get
  put (f s)
```

Przykład użycia:
```
> runState (modify (+10)) 5 = (15, ())
```

## Przykład 1: Licznik Porównań w Algorytmie Quicksort

Wykorzystajmy monadę State do zliczania liczby porównań wykonywanych podczas sortowania metodą quicksort:

```haskell
type Counter = State Int

-- Funkcja inkrementująca licznik
incCounter :: Counter ()
incCounter = modify (+1)

-- Funkcja porównująca dwie wartości i inkrementująca licznik
compareAndCount :: Ord a => a -> a -> Counter Bool
compareAndCount x y = do
  incCounter
  return (x <= y)

-- Implementacja quicksort z użyciem monady Counter
quicksort :: Ord a => [a] -> Counter [a]
quicksort [] = return []
quicksort (x:xs) = do
  -- Dzielimy listę na elementy mniejsze i większe/równe od pivota
  lesser <- filterM (\y -> compareAndCount y x) xs
  greater <- filterM (\y -> do
                        result <- compareAndCount x y
                        return (not result)) xs
  
  -- Sortujemy rekurencyjnie obie podlisty
  sortedLesser <- quicksort lesser
  sortedGreater <- quicksort greater
  
  -- Łączymy posortowane podlisty
  return (sortedLesser ++ [x] ++ sortedGreater)
```

Przykład użycia:
```
> runState (quicksort [3, 1, 4, 1, 5, 9, 2, 6]) 0
```

Wynik zawiera posortowaną listę oraz liczbę wykonanych porównań.

## Przykład 2: Gra RPG

Wykorzystajmy monadę State do modelowania prostej gry RPG, gdzie postać posiada poziom doświadczenia, punkty życia i złoto:

```haskell
data GameState = GameState {
  experience :: Int,
  health :: Int,
  gold :: Int
} deriving (Show)

type Game = State GameState

-- Funkcja zwiększająca doświadczenie postaci
gainExperience :: Int -> Game ()
gainExperience exp = modify $ \state ->
  state { experience = experience state + exp }

-- Funkcja zadająca obrażenia postaci
-- Zwraca True jeśli postać nadal żyje
takeDamage :: Int -> Game Bool
takeDamage amount = do
  state <- get
  let newHealth = max 0 (health state - amount)
  put $ state { health = newHealth }
  return (newHealth > 0)

-- Funkcja dodająca złoto do ekwipunku
collectGold :: Int -> Game Int
collectGold amount = do
  state <- get
  let newGold = gold state + amount
  put $ state { gold = newGold }
  return newGold
```

Przykładowe użycie:
```haskell
runState (do
    gainExperience 100
    alive <- takeDamage 10
    if alive 
        then collectGold 50
        else return 0
) (GameState 0 100 0)
-- Wynik: (GameState {experience = 100, health = 90, gold = 50}, 50)
```

## Przykład 3: Nawigator po Labiryncie

Wykorzystajmy monadę State do zaimplementowania nawigatora po labiryncie, który śledzi aktualną pozycję i odwiedzone komórki:

```haskell
type Position = (Int, Int)
type MazeMap = [[Char]]  -- '#' dla ścian, '.' dla pustych przestrzeni, 'S' dla startu, 'E' dla wyjścia

data MazeState = MazeState {
  position :: Position,
  visited :: [Position],
  maze :: MazeMap,
  pathFound :: Bool
} deriving (Show)

type MazeNavigator = State MazeState

data Direction = North | East | South | West
  deriving (Show, Eq)

-- Funkcja próbująca wykonać ruch w danym kierunku
move :: Direction -> MazeNavigator Bool
move dir = do
  state <- get
  let currentPos = position state
      newPos = movePosition currentPos dir
      mazeMap = maze state
  
  if isValidMove mazeMap newPos && notElem newPos (visited state) then do
    put $ state { 
      position = newPos, 
      visited = newPos : visited state,
      pathFound = pathFound state || isExit mazeMap newPos
    }
    return True
  else
    return False

-- Funkcja pomocnicza: oblicza nową pozycję po wykonaniu ruchu w danym kierunku
movePosition :: Position -> Direction -> Position
movePosition (x, y) North = (x, y-1)
movePosition (x, y) South = (x, y+1)
movePosition (x, y) East = (x+1, y)
movePosition (x, y) West = (x-1, y)

-- Sprawdza czy ruch jest dozwolony (w granicach labiryntu i nie na ścianę)
isValidMove :: MazeMap -> Position -> Bool
isValidMove maze (x, y) =
  y >= 0 && y < length maze &&
  x >= 0 && x < length (maze !! y) &&
  (maze !! y) !! x /= '#'

-- Sprawdza czy pozycja jest wyjściem z labiryntu
isExit :: MazeMap -> Position -> Bool
isExit maze (x, y) = (maze !! y) !! x == 'E'

-- Próbuje wykonać ruch w danym kierunku i kontynuuje szukanie ścieżki
tryDirection :: Direction -> MazeNavigator Bool
tryDirection dir = do
  state <- get
  
  -- Jeśli już znaleźliśmy ścieżkę, zwracamy True
  if pathFound state then
    return True
  else do
    -- Zapamiętujemy aktualny stan przed próbą ruchu
    let oldState = state
    
    -- Próbujemy wykonać ruch
    success <- move dir
    
    if success then do
      -- Jeśli ruch się powiódł, sprawdzamy czy to wyjście
      newState <- get
      
      if isExit (maze newState) (position newState) then
        -- Znaleźliśmy wyjście!
        put $ newState { pathFound = True }
        >> return True
      else do
        -- Kontynuujemy poszukiwanie ścieżki
        found <- findPath
        
        if found then
          return True
        else do
          -- Wracamy do poprzedniego stanu (backtracking)
          put oldState
          return False
    else
      -- Ruch się nie powiódł
      return False

-- Znajduje ścieżkę przez labirynt używając przeszukiwania w głąb (DFS)
findPath :: MazeNavigator Bool
findPath = do
  state <- get
  
  -- Jeśli już znaleźliśmy ścieżkę, zwracamy True
  if pathFound state then
    return True
  else do
    -- Próbujemy wykonać ruch w każdym z czterech kierunków
    north <- tryDirection North
    if north then return True else do
      east <- tryDirection East
      if east then return True else do
        south <- tryDirection South
        if south then return True else do
          west <- tryDirection West
          west  -- Zwracamy wynik ostatniej próby

-- Inicjalizuje stan labiryntu na podstawie mapy
initMazeState :: MazeMap -> MazeState
initMazeState mazeMap = 
  let startPos = findStart mazeMap
  in MazeState startPos [startPos] mazeMap False

-- Znajduje pozycję startową w labiryncie
findStart :: MazeMap -> Position
findStart maze = head [(x, y) | y <- [0..length maze - 1], 
                                x <- [0..length (maze !! y) - 1], 
                                (maze !! y) !! x == 'S']

-- Przykładowy labirynt
exampleMaze :: MazeMap
exampleMaze = [
  "##########",
  "#S.......#",
  "#.######.#",
  "#....#...#",
  "####.#.###",
  "#....#...#",
  "#.######.#",
  "#........E",
  "##########"
  ]
```

Przykładowe użycie:
```haskell
> runState findPath (initMazeState exampleMaze)
```

Wynik zawiera stan po zakończeniu poszukiwania ścieżki, w tym informację czy udało się znaleźć wyjście.
