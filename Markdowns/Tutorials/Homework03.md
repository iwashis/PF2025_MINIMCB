# Zadania domowe z Haskella, część 3: Monada State i Symulacja Gry "Poszukiwacze Skarbów"

## Cel zadania
Zaimplementuj system symulacji interaktywnej gry przygodowej "Poszukiwacze Skarbów" z wykorzystaniem monady State oraz IO. Zamiast korzystać z generatora liczb losowych, poproś użytkownika o podanie wyniku rzutu kośćmi oraz decyzji w punktach wyboru.

## Opis gry
"Poszukiwacze Skarbów" to gra planszowa, w której:
- Plansza to mapa z kilkoma ścieżkami prowadzącymi do skarbu
- Gracz rozpoczyna przygodę na pozycji startowej i musi dotrzeć do skarbu
- Na planszy znajdują się:
  - Punkty Decyzji - miejsca, gdzie gracz może wybrać jedną z kilku dostępnych ścieżek
  - Przeszkody - które mogą cofnąć gracza lub opóźnić jego podróż
  - Skarby Pośrednie - dające graczowi dodatkowe punkty
  - Pułapki - które mogą odebrać graczowi zgromadzone punkty
- Gracz ma określoną ilość energii, która zmniejsza się przy każdym ruchu
- Celem jest dotarcie do skarbu głównego z jak największą liczbą punktów i przed wyczerpaniem energii

## Wymagania

Zdefiniuj typ `GameState` reprezentujący stan gry oraz typ `AdventureGame a = StateT GameState IO a` reprezentujący operacje w kontekście gry. Następnie zaimplementuj funkcje:

- `movePlayer :: Int -> AdventureGame Int` - przesuwa gracza na podstawie podanego wyniku rzutu i zwraca liczbę pól, o które się przesunął
- `makeDecision :: [String] -> AdventureGame String` - obsługuje punkt decyzji, prezentując graczowi opcje i zwracając jego wybór
- `handleLocation :: AdventureGame Bool` - obsługuje aktualną lokację gracza (przeszkoda, skarb, pułapka), zwraca True jeśli gracz dotarł do celu
- `playTurn :: AdventureGame Bool` - obsługuje jedną turę gry, zwraca True jeśli gra się zakończyła
- `playGame :: AdventureGame ()` - prowadzi grę aż do jej zakończenia, wyświetlając stan gry po każdym ruchu

Dodatkowo zaimplementuj funkcje IO do interakcji z użytkownikiem:
- `getDiceRoll :: IO Int` - prosi użytkownika o podanie wyniku rzutu kośćmi
- `displayGameState :: GameState -> IO ()` - wyświetla aktualny stan gry
- `getPlayerChoice :: [String] -> IO String` - prosi użytkownika o wybór jednej z opcji

## Kod początkowy

```haskell
module TreasureHunters where

import Control.Monad.State
import Control.Monad.IO.Class
import System.IO
import Data.List 
-- Typ reprezentujący lokację na planszy
data LocationType = 
    Empty           -- Zwykłe pole
  | Decision        -- Punkt decyzji
  | Obstacle Int    -- Przeszkoda (z wartością opóźnienia)
  | Treasure Int    -- Skarb (z wartością punktów)
  | Trap Int        -- Pułapka (z wartością ujemną punktów)
  | Goal            -- Cel gry - główny skarb
  deriving (Show, Eq)

-- Typ reprezentujący lokację
data Location = Location {
  locationType :: LocationType,
  description :: String,
  connections :: [Int]  -- Indeksy połączonych lokacji
} deriving (Show)

-- Typ reprezentujący stan gry
data GameState = GameState {
  playerPosition :: Int,     -- Aktualna pozycja gracza
  playerEnergy :: Int,       -- Pozostała energia gracza
  playerScore :: Int,        -- Wynik gracza
  gameMap :: [Location],     -- Mapa gry
  visitedLocations :: [Int], -- Odwiedzone lokacje
  turns :: Int               -- Liczba wykonanych tur
} deriving (Show)

-- Definicja typu dla gry przygodowej
type AdventureGame a = StateT GameState IO a

-- Funkcja przesuwająca gracza na podstawie wyniku rzutu
-- Zwraca liczbę pól, o które przesunął się gracz
movePlayer :: Int -> AdventureGame Int
movePlayer = undefined

-- Funkcja obsługująca punkt decyzji
-- Prezentuje graczowi opcje i zwraca jego wybór
makeDecision :: [String] -> AdventureGame String
makeDecision = undefined

-- Funkcja obsługująca aktualną lokację gracza
-- Zwraca True jeśli gracz dotarł do celu
handleLocation :: AdventureGame Bool
handleLocation = undefined

-- Funkcja obsługująca jedną turę gry
-- Zwraca True jeśli gra się zakończyła
playTurn :: AdventureGame Bool
playTurn = undefined

-- Funkcja prowadząca grę aż do jej zakończenia
playGame :: AdventureGame ()
playGame = undefined

-- Funkcja pobierająca od użytkownika wynik rzutu kośćmi
getDiceRoll :: IO Int
getDiceRoll = undefined

-- Funkcja wyświetlająca aktualny stan gry
displayGameState :: GameState -> IO ()
displayGameState = undefined

-- Funkcja pobierająca wybór gracza spośród dostępnych opcji
getPlayerChoice :: [String] -> IO String
getPlayerChoice = undefined

-- Funkcja tworząca przykładową mapę gry
createGameMap :: [Location]
createGameMap = [
  -- Lokacja 0: Start
  Location Empty "Początek Twojej przygody. Ścieżka prowadzi w głąb lasu." [1],
  
  -- Lokacja 1: Rozwidlenie dróg
  Location Decision "Rozwidlenie dróg. Możesz pójść leśną ścieżką lub górskim szlakiem." [2, 3],
  
  -- Lokacje 2-..: Reszta mapy do zdefiniowania
  -- ...
  
  -- Przykłady innych lokacji:
  -- Location (Obstacle 2) "Zwalony pień drzewa blokuje drogę. Musisz go obejść." [4, 5],
  -- Location (Treasure 10) "Znalazłeś stary kufer z monetami! (+10 punktów)" [6],
  -- Location (Trap 5) "Wpadłeś w ruchome piaski! Tracisz część ekwipunku. (-5 punktów)" [7],
  -- Location Goal "Dotarłeś do ukrytej świątyni! Twój cel znajduje się przed tobą." []
  
  -- Dokończ mapę samodzielnie...
]

-- Funkcja main do uruchomienia gry
main :: IO ()
main = do
  putStrLn "Poszukiwacze Skarbów"
  putStrLn "===================="
  putStrLn "Rozpoczynasz poszukiwanie legendarnego skarbu ukrytego w starożytnej świątyni."
  putStrLn "Masz ograniczoną energię, więc wybieraj mądrze swoją trasę!"
  
  -- Inicjalizacja gry
  let initialState = GameState {
    playerPosition = 0,
    playerEnergy = 20,
    playerScore = 0,
    gameMap = createGameMap,
    visitedLocations = [],
    turns = 0
  }
  
  do 
    runStateT playGame initialState 
    return ()
  
  putStrLn "Dziękujemy za grę!"
```

## Przykładowe uruchomienie
```
$ runhaskell TreasureHunters.hs
Poszukiwacze Skarbów
====================
Rozpoczynasz poszukiwanie legendarnego skarbu ukrytego w starożytnej świątyni.
Masz ograniczoną energię, więc wybieraj mądrze swoją trasę!

Tura: 1 | Energia: 20 | Punkty: 0
Lokalizacja: Początek Twojej przygody. Ścieżka prowadzi w głąb lasu.
Podaj wynik rzutu kośćmi (1-6): 4

Przesunąłeś się o 1 pole.

Tura: 2 | Energia: 16 | Punkty: 0
Lokalizacja: Rozwidlenie dróg. Możesz pójść leśną ścieżką lub górskim szlakiem.
Wybierz drogę:
1. Leśna ścieżka
2. Górski szlak
Twój wybór: 1

Wybrałeś: Leśna ścieżka

[...]

Tura: 8 | Energia: 3 | Punkty: 25
Lokalizacja: Dotarłeś do ukrytej świątyni! Twój cel znajduje się przed tobą.
Gratulacje! Odnalazłeś legendarny skarb!
Twój wynik końcowy: 75 punktów w 8 turach.
Dziękujemy za grę!
```

## Wskazówki
1. Wykorzystaj monady StateT do połączenia funkcjonalności State (śledzenie stanu gry) i IO (interakcja z użytkownikiem).
2. Funkcja `lift` z modułu Control.Monad.IO.Class pozwala wykonywać operacje IO wewnątrz monady AdventureGame.
3. Pomyśl o różnych efektach dla różnych typów lokacji:
   - W punktach decyzji (Decision) użyj funkcji `makeDecision` aby umożliwić graczowi wybór drogi
   - Na przeszkodach (Obstacle) zmniejsz energię gracza lub opóźnij jego ruch
   - Przy skarbach (Treasure) zwiększ wynik gracza
   - Przy pułapkach (Trap) zmniejsz wynik gracza
4. Pamiętaj o walidacji danych wejściowych w funkcjach `getDiceRoll` i `getPlayerChoice`.
5. Dodaj efekty wizualne poprzez odpowiednie formatowanie tekstu, aby gra była bardziej wciągająca.
6. Dokończ implementację mapy, dodając różne ścieżki do celu z różnymi wyzwaniami.
