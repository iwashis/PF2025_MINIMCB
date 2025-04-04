# Monada State i Symulacja Gry Planszowej z IO

## Cel zadania

Zaimplementuj system symulacji prostej gry planszowej z wykorzystaniem monady State oraz IO. Zamiast korzystać z generatora liczb losowych, poproś użytkownika o podanie wyniku rzutu kośćmi.

## Opis gry

Stwórz symulację gry planszowej z następującą strukturą:
- Plansza ma określoną długość (liczbę pól)
- Gracz zaczyna na pozycji 0 i jego celem jest dotarcie do końca planszy
- W każdej turze gracz podaje wynik rzutu kośćmi (suma oczek) i przesuwa się o podaną liczbę pól
- Na planszy znajdują się przeszkody na określonych polach - jeśli gracz trafi na przeszkodę, nie przesuwa się w tej turze

## Wymagania

Zdefiniuj typ `GameBoard` reprezentujący stan planszy oraz typ `BoardGame a = StateT GameBoard IO a` reprezentujący operacje w kontekście gry. Następnie zaimplementuj funkcje:
- `movePlayer :: Int -> BoardGame Int` - przesuwa gracza na podstawie podanego wyniku rzutu i zwraca liczbę pól, o które się przesunął
- `playTurn :: BoardGame Bool` - obsługuje jedną turę gry, zwraca True jeśli gracz dotarł do końca planszy
- `playGame :: BoardGame ()` - prowadzi grę aż do jej zakończenia, wyświetlając stan planszy po każdym ruchu

Dodatkowo zaimplementuj funkcje IO do interakcji z użytkownikiem:
- `getDiceRoll :: IO Int` - prosi użytkownika o podanie wyniku rzutu kośćmi
- `displayBoard :: GameBoard -> IO ()` - wyświetla aktualny stan planszy

## Kod początkowy

```haskell
module BoardGameSimulation where

import Control.Monad.State
import Control.Monad.IO.Class
import System.IO

-- Typ reprezentujący stan gry planszowej
data GameBoard = GameBoard {
  playerPosition :: Int,  -- Aktualna pozycja gracza
  boardSize :: Int,       -- Rozmiar planszy
  obstacles :: [Int]      -- Pozycje przeszkód
} deriving (Show)

-- Definicja typu dla gry planszowej
type BoardGame a = StateT GameBoard IO a

-- Funkcja przesuwająca gracza na podstawie wyniku rzutu
-- Zwraca liczbę pól, o które przesunął się gracz
movePlayer :: Int -> BoardGame Int
movePlayer = undefined

-- Funkcja obsługująca jedną turę gry
-- Zwraca True jeśli gracz dotarł do końca planszy
playTurn :: BoardGame Bool
playTurn = undefined

-- Funkcja prowadząca grę aż do jej zakończenia
playGame :: BoardGame ()
playGame = undefined

-- Funkcja pobierająca od użytkownika wynik rzutu kośćmi
getDiceRoll :: IO Int
getDiceRoll = undefined

-- Funkcja wyświetlająca aktualny stan planszy
displayBoard :: GameBoard -> IO ()
displayBoard = undefined

-- Funkcja main do uruchomienia gry
main :: IO ()
main = do
  putStrLn "Symulacja gry planszowej"
  putStrLn "========================"
  putStrLn "Rozpoczynamy grę na planszy o długości 30 pól z przeszkodami na polach 5, 10, 15, 20 i 25."
  void $ runStateT playGame (GameBoard 0 30 [5, 10, 15, 20, 25])
  putStrLn "Dziękujemy za grę!"
  where void = (>> return ())
```

## Przykładowe uruchomienie

```
$ runhaskell BoardGameSimulation.hs
Symulacja gry planszowej
========================
Rozpoczynamy grę na planszy o długości 30 pól z przeszkodami na polach 5, 10, 15, 20 i 25.
Aktualna pozycja: 0, Cel: 30
Podaj wynik rzutu kośćmi (2-12): 7
Przesunąłeś się o 7 pól.
Aktualna pozycja: 7, Cel: 30
Podaj wynik rzutu kośćmi (2-12): 8
Przesunąłeś się o 8 pól.
Aktualna pozycja: 15, Cel: 30
Trafiłeś na przeszkodę! Nie przesuwasz się w tej turze.
...
Gratulacje! Dotarłeś do końca planszy w 6 turach.
Dziękujemy za grę!
```

## Wskazówki

1. Użyj monady StateT do połączenia funkcjonalności State (śledzenie stanu gry) i IO (interakcja z użytkownikiem).
2. Funkcja `lift` z modułu Control.Monad.IO.Class pozwala wykonywać operacje IO wewnątrz monady BoardGame.
3. Zwróć uwagę na obsługę przeszkód - sprawdzaj, czy nowa pozycja gracza nie znajduje się na liście przeszkód.
4. Pamiętaj o walidacji danych wejściowych w funkcji `getDiceRoll`.
