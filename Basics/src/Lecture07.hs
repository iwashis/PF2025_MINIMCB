module Lecture07 (get, put) where 


-- Celem tego wykladu jest kontynuacja wykladu Lecture05, gdzie wprowadzono monade State s
import Lecture05  -- Importujemy moduł Lecture05, który zawiera definicję monady State
-- Krotkie przypomnienie
-- f :: a -> State s b 
-- ---------------------
-- a -> (s -> (s,b))
-- a -> s -> (s,b)
-- ---------------------
-- f' :: (s,a) -> (s,b)
-- a ───┐                 ┌─── b
--      │                 │
--      └──→[      f    ]─┘
--      │                 │
-- s ───┘                 └─── s
--

-- f1 :: a -> State s b 
-- f2 :: b -> State s c 
-- f3 :: c -> State s d
-- f1 >=> f2 >=> f3 moze byc interpretowany diagramatycznie jako
-- a ───┐                 ┌─── b      b ───┐                 ┌─── c      c ───┐                 ┌─── d
--      │                 │                │                 │                │                 │
--      └──→[     f1    ]─┘                └──→[     f2    ]─┘                └──→[     f3    ]─┘
--      │                 │                │                 │                │                 │
-- s ───┘                 └─── s      s ───┘                 └─── s      s ───┘                 └─── s
-- Słownie, funkcje o typie
-- a -> State s b
-- to funkcje ktore przyjmują na wejściu element a i stan typu s i zwracają nowy stan typu s oraz wartość typu b.
--
-- Dodatkowe funkcjonalnosci

-- Funkcja get pozwala na odczytanie aktualnego stanu.
-- W monadzie stanu, get zwraca aktualny stan bez jego modyfikacji.
-- 
-- Typ: State s s oznacza obliczenie stanowe, które:
-- - Przyjmuje stan typu s
-- - Zwraca wartość typu s (tę samą co stan)
-- - Nie modyfikuje stanu
--                        ┌─── s
--                        │
--       ──→[    get    ]─┘
--      │                 │
-- s ───┘                 └─── s
-- Przykład użycia:
-- runState get 5 = (5, 5)
-- Pierwsza wartość (5) to nowy stan (niezmieniony)
-- Druga wartość (5) to zwrócona wartość (aktualny stan)
get :: State s s 
get = State $ \s -> (s,s) -- newtype State s a = State { runState : s -> (s,a)} 

-- Funkcja put pozwala na ustawienie nowego stanu.
-- W monadzie stanu, put przyjmuje nowy stan i go ustawia.
--
-- Typ: s -> State s () oznacza obliczenie stanowe, które:
-- - Przyjmuje wartość typu s (nowy stan)
-- - Modyfikuje stan na podaną wartość
-- - Zwraca () (nic istotnego)
--
-- s1 :: s  ──┐                 ┌─── ()
--            │                 │
--            └──→[    put    ]─┘
--            │                 │
-- s2 :: s ───┘                 └─── s1 :: s
--
-- Przykład użycia:
-- runState (put 10) 5 = (10, ())
-- Pierwsza wartość (10) to nowy stan (został zmieniony z 5 na 10)
-- Druga wartość (()) to zwrócona wartość (w tym przypadku nic istotnego)
put :: s -> State s ()
put state = State $ const (state, ()) 
-- const definiujemy następująco:
-- const :: b -> (a -> b)
-- const x = \_ -> x
--
-- Funkcja modify pozwala na modyfikację aktualnego stanu za pomocą funkcji.
-- Przykład użycia:
-- runState (modify (+10)) 5 = (15, ())
modify :: (s -> s) -> State s ()
modify f = State $ \s -> ( f s, ())  


-- Przyklad 1.
-- Zaimplementuj prostą monadę stanową reprezentującą licznik. 
-- Dodaj funkcje zwiększające i zmniejszające licznik oraz funkcję zwracającą aktualny stan licznika.
-- Uzyj tej monady do napisania funkcji quicksort :: (Ord a) => [a] -> Counter [a]
-- ktora sortuje wejsciowa liste, a w stanie zwraca licznik zawierajacy liczbe wykonanych porownan.
-- quicksort :: (Ord a) => [a] -> [a]
-- quicksort [] = []
-- quicksort (x : xs) = quicksort le ++ [x] ++ quicksort gr
--   where
--     le = filter (< x) xs     -- elementy mniejsze od pivota
--     gr = filter (>= x) xs    -- elementy większe lub równe pivotowi
--
type Counter = State Int
quicksort :: Ord a => [a] -> Counter [a]
-- quicksort : [a] -> State Int [a]
--  [a] -> Int -> (Int, [a])
-- (Int, [a]) -> (Int, [a])
quicksort [] = return [] -- State \s -> (s, [])
quicksort (x:xs) = do
  l <- quicksort le 
  g <- quicksort gr 
  -- s <- get 
  -- put $ s + numberOfComp
  modify (+ numberOfComp)
  pure $ l ++ [x] ++ g
  where
    le = filter (< x) xs     -- elementy mniejsze od pivota
    gr = filter (>= x) xs    -- elementy większe lub równe pivotowi
    numberOfComp = 2 * length xs

filterM :: (Monad m) =>  (a -> m Bool) -> [a] -> m [a]
filterM _ [] = pure []
filterM f (x:xs) = do 
  isTrue <- f x 
  ys <- filterM f xs
  if isTrue then pure (x:ys) else pure ys 

quicksort' :: Ord a => [a] -> Counter [a]
quicksort' [] = pure [] 
quicksort' (x:xs) = do 
  l <- filterM lessThanX xs 
  g <- filterM greaterThanX xs 
  left <- quicksort' l 
  right <- quicksort' g
  pure $ left ++ [x] ++ right
  where 
    lessThanX y = do 
      modify (+1)
      pure (y < x) -- Counter Bool
    greaterThanX y  = do 
     modify (+1)
     pure (y >= x) 

    
    




-- Przyklad 2.
-- Zaprojektuj monadę stanową reprezentującą prostą grę RPG, 
-- w której postać posiada poziom doświadczenia, punkty życia i złoto.
-- Przykładowe użycie:
-- runState (do
--     gainExperience 100
--     alive <- takeDamage 10
--     if alive 
--         then collectGold 50
--         else return 0
-- ) (GameState 0 100 0)
-- Wynik: (GameState {experience = 100, health = 90, gold = 50}, 50)



-- Definicja stanu gry
data GameState = GameState { 
    experience :: Int, 
    health :: Int, 
    gold :: Int
  } 
  deriving Show

type Game = State GameState

-- Funkcja zwiększająca doświadczenie postaci
gainExperience :: Int -> Game ()
gainExperience exp = modify f  
  where 
    f :: GameState -> GameState 
    f (GameState e h g) = GameState (e + exp) h g -- da sie troszke ladniej zapisac korzystajac z rozszerzen jezyka


-- Funkcja zadająca obrażenia postaci
-- Zwraca True jeśli postać nadal żyje
takeDamage :: Int -> Game Bool
takeDamage d = do 
  modify f 
  gameState <- get 
  pure $ health gameState > 0
  where 
   f (GameState e h g) = GameState e (h - d) g

-- Funkcja dodająca złoto do ekwipunku
collectGold :: Int -> Game Int
collectGold additionalGold = do  -- Zwraca aktualną ilość złota
  modify f
  gameState <- get 
  pure $ gold gameState
  where 
    f (GameState e h g ) = GameState e h (g + additionalGold)


gameExample = runState (do
    gainExperience 100
    alive <- takeDamage 10
    if alive 
        then collectGold 50
        else return 0
    ) (GameState 0 100 0)

-- Przykład 3.
-- Nawigator po labiryncie z użyciem monady State
-- Wykorzystujemy State do śledzenia pozycji i odwiedzonych komórek podczas eksploracji labiryntu

-- Definicja typów
type Position = (Int, Int)
type MazeMap = [[Char]]  -- '#' dla ścian, '.' dla pustych przestrzeni, 'S' dla startu, 'E' dla wyjścia

-- Stan labiryntu zawiera aktualną pozycję, listę odwiedzonych komórek, mapę labiryntu
-- oraz flagę określającą czy znaleziono ścieżkę
data MazeState = MazeState {
  position :: Position,
  visited :: [Position],
  maze :: MazeMap,
  pathFound :: Bool
} deriving (Show)

type MazeNavigator = State MazeState

-- Kierunki ruchu
data Direction = North | East | South | West
  deriving (Show, Eq)

-- Funkcja próbująca wykonać ruch w danym kierunku
-- Zwraca True jeśli ruch był możliwy, False w przeciwnym wypadku
-- Aktualizuje stan jeśli ruch był możliwy
move :: Direction -> MazeNavigator Bool
move dir = undefined

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

-- Znajduje ścieżkę przez labirynt używając przeszukiwania w głąb (DFS)
-- Zwraca True jeśli znaleziono ścieżkę, False w przeciwnym wypadku
findPath :: MazeNavigator Bool
findPath = undefined

-- Próbuje wykonać ruch w danym kierunku i kontynuuje szukanie ścieżki
-- Implementuje mechanizm nawrotów (backtracking)
tryDirection :: Direction -> MazeNavigator Bool
tryDirection dir = undefined

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

-- Przykładowe użycie:
-- runState findPath (initMazeState exampleMaze)

