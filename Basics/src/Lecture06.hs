module Lecture06 where 
import Lecture05  -- Importujemy moduł Lecture05, który zawiera definicję monady State

-- Funkcja get pozwala na odczytanie aktualnego stanu.
-- W monadzie stanu, get zwraca aktualny stan bez jego modyfikacji.
-- 
-- Typ: State s s oznacza obliczenie stanowe, które:
-- - Przyjmuje stan typu s
-- - Zwraca wartość typu s (tę samą co stan)
-- - Nie modyfikuje stanu
--
-- Przykład użycia:
-- runState get 5 = (5, 5)
-- Pierwsza wartość (5) to nowy stan (niezmieniony)
-- Druga wartość (5) to zwrócona wartość (aktualny stan)
get :: State s s 
get = undefined 

-- Funkcja put pozwala na ustawienie nowego stanu.
-- W monadzie stanu, put przyjmuje nowy stan i go ustawia.
--
-- Typ: s -> State s () oznacza obliczenie stanowe, które:
-- - Przyjmuje wartość typu s (nowy stan)
-- - Modyfikuje stan na podaną wartość
-- - Zwraca () (nic istotnego)
--
-- Przykład użycia:
-- runState (put 10) 5 = (10, ())
-- Pierwsza wartość (10) to nowy stan (został zmieniony z 5 na 10)
-- Druga wartość (()) to zwrócona wartość (w tym przypadku nic istotnego)
put :: s -> State s ()
put newState = undefined 

-- Funkcja modify pozwala na modyfikację aktualnego stanu za pomocą funkcji.
-- Przykład użycia:
-- runState (modify (+10)) 5 = (15, ())
modify :: (s -> s) -> State s ()
modify f = undefined 


-- Przyklad 1.
-- Zaimplementuj prostą monadę stanową reprezentującą licznik. 
-- Dodaj funkcje zwiększające i zmniejszające licznik oraz funkcję zwracającą aktualny stan licznika.
-- Uzyj tej monady do napisania funkcji quicksort :: (Ord a) => [a] -> Counter [a]
-- ktora sortuje wejsciowa liste, a w stanie zwraca licznik zawierajacy liczbe wykonanych porownan.











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
data GameState -- = ...

type Game = State GameState

-- Funkcja zwiększająca doświadczenie postaci
gainExperience :: Int -> Game ()
gainExperience = undefined

-- Funkcja zadająca obrażenia postaci
-- Zwraca True jeśli postać nadal żyje
takeDamage :: Int -> Game Bool
takeDamage = undefined 

-- Funkcja dodająca złoto do ekwipunku
collectGold :: Int -> Game Int
collectGold = undefined  -- Zwraca aktualną ilość złota



-- Przyklad 3.
-- Typ kalkulatora to State z liczbą zmiennoprzecinkową jako stanem
-- Przykładowe użycie:
-- runState (do
--     add 10
--     multiply 2
--     subtract 5
--     divide 3
-- ) 0
-- Wynik: (5.0, 5.0)

type Calculator = State Double

-- Podstawowe operacje arytmetyczne
add :: Double -> Calculator Double
add = undefined 

subtract :: Double -> Calculator Double
subtract = undefined 

multiply :: Double -> Calculator Double
multiply = undefined 

divide :: Double -> Calculator Double
divide = undefined -- Funkcja resetująca stan kalkulatora

clear :: Calculator ()
clear = undefined

-- Funkcja wykonująca sekwencję operacji
calculate :: [Calculator Double] -> Calculator Double
calculate = undefined               -- Zwraca końcowy wynik


