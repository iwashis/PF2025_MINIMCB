module Tutorials06 where 

import Control.Monad.State (State(..), get, put, evalState)

--
-- 1. **Algorytm losowy z użyciem State**
--  Zaimplementuj prosty generator liczb pseudolosowych przy użyciu monady State. Zdefiniuj funkcję 
-- `randomInt :: Int -> Int -> State Int Int`, która generuje liczbę całkowitą z podanego zakresu [a,b], 
-- używając [liniowego generatora kongruencyjnego](https://en.wikipedia.org/wiki/Linear_congruential_generator). 
-- Następnie napisz funkcję `randomList :: Int -> Int -> Int -> State Int [Int]`, 
-- która generuje listę n losowych liczb z zakresu [a,b]. Użyj funkcji `evalState` do uruchomienia obliczeń z określonym ziarnem (seed).

type Random = State Int 

randomInt :: Int -> Int -> Random Int
randomInt x y = do 
  s <- get 
  let s' = ( a * s + c ) `mod` m 
  put s'
  pure $ x + s `mod` (y - x)
  where 
    a = 22695477 
    c = 1
    m = 2 ^ 31

exampleRandomInt :: Int
exampleRandomInt = evalState (randomInt 10 15) 152

randomList :: Int -> Int -> Int -> Random [Int]
randomList 0 _ _ = pure [] 
randomList n a b = do 
  i <- randomInt a b
  is <- randomList (n-1) a b
  pure (i:is)

exampleRandomList :: [Int]
exampleRandomList = evalState (randomList 12 10 100) 152


-- 2. **Drzewo binarne i etykietowanie z State**
--    Zdefiniuj typ drzewa binarnego `data Tree a = Empty | Node a (Tree a) (Tree a)`. 
--    Następnie zaimplementuj funkcję `labelTree :: Tree a -> State Int (Tree (a, Int))`, 
--    która etykietuje każdy węzeł drzewa unikalnym numerem, używając monady State do śledzenia licznika. 
--    Numeracja powinna być w porządku preorder. Napisz również funkcję `countNodes :: Tree a -> State (Sum Int) (Tree a)`, która liczy węzły w drzewie, używając monady State do akumulacji.
--
-- 3. **Interaktywna kalkulacja z użyciem IO**
--    Napisz program `calculator :: IO ()`, który wczytuje od użytkownika dwie liczby oraz operację (dodawanie, odejmowanie, mnożenie, dzielenie) i wypisuje wynik. 
--    Program powinien obsługiwać błędy (np. dzielenie przez zero) i pytać użytkownika, czy chce kontynuować obliczenia. 
--    Użyj funkcji `getLine`, `readLn` oraz `putStrLn` do interakcji z użytkownikiem.

