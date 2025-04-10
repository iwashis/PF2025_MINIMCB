module Tutorials05 where

import Control.Monad.Writer (Writer,tell)
import Data.Monoid (Sum(..))
import Lecture05
import Lecture07

--
-- 1. **Podstawy Monady Maybe**  
--
--    Napisz funkcję `safeDivide :: Int -> Int -> Maybe Int`, która wykonuje bezpieczne dzielenie dwóch liczb całkowitych. 
--    Jeżeli dzielnik jest równy zero, funkcja powinna zwrócić `Nothing`, w przeciwnym razie `Just` z wynikiem dzielenia.
--    Następnie zaimplementuj funkcję `chainedDivision :: [Int] -> Maybe Int`, która przeprowadza sekwencję bezpiecznych 
--    dzieleń, zaczynając od pierwszej liczby i dzieląc ją przez każdą kolejną. Użyj operatora `>>=` (bind).
--


safeDivide :: Int -> Int -> Maybe Int 
safeDivide _ 0 = Nothing
safeDivide m n = Just (m `div` n)

chainedDivision [] = Nothing
chainedDivision [x] = Just x 
-- chainedDivision (x:y:ys) = safeDivide x y >>= \z -> chainedDivision (z:ys)   
chainedDivision (x:y:ys) = do 
  z <- safeDivide x y 
  chainedDivision (z:ys)
  
-- 2. **List Monada i eksploracja ścieżek**  
--
--    Napisz funkcję `knights :: (Int, Int) -> [(Int, Int)]`, która dla danej pozycji skoczka na szachownicy 
--    zwraca listę wszystkich możliwych ruchów skoczka. Następnie zaimplementuj funkcję 
--    `knightPaths :: Int -> (Int, Int) -> (Int, Int) -> [[(Int, Int)]]`, 
--    która znajduje wszystkie możliwe ścieżki o długości `n` ruchów z jednej pozycji do drugiej. Wykorzystaj monadę list oraz 
--    operator `>>=` do eksploracji wszystkich możliwych ścieżek.
--
--    Helper:
--     [(x+2, y+1), (x+2, y-1), (x-2, y+1), (x-2, y-1), (x+1, y+2), (x+1, y-2), (x-1, y+2), (x-1, y-2)]

knights (x,y) = filter onBoard jumps
-- [(a,b) | (a,b) <- jumps, a > 0 , a <= 8, b > 0, b <= 8 ] 
  where 
    jumps = [(x+2, y+1), (x+2, y-1), (x-2, y+1), (x-2, y-1), (x+1, y+2), (x+1, y-2), (x-1, y+2), (x-1, y-2)]
    onBoard (a,b) = (a>0) && (b > 0) && (a<= 8) && (b<=8)

knightPaths :: Int -> (Int, Int) -> (Int, Int) -> [[(Int, Int)]]
knightPaths 0 (x,y) (a,b) = [[(x,y)] | (x,y) == (a,b)] 
knightPaths n (x,y) (a,b) = do 
  (x',y') <- knights (x,y)
  path <- knightPaths (n-1) (x',y') (a,b)
  return $ (x,y) : path



-- 3. **Monada State do śledzenia stanu**  
--
--    Zdefiniuj funkcję `runningSum :: [Int] -> [Int]`, która dla listy liczb całkowitych zwraca listę sum częściowych.
--    Przykładowo, dla listy `[1, 2, 3, 4]` wynikiem powinno być `[1, 3, 6, 10]`. Zaimplementuj tę funkcję 
--    używając monady State, korzystając z operacji `get`, `put` i funkcji `runState` lub `evalState`.

type Counter = State Int 
-- a -> Counter b 
-- a -> Int -> (b, Int)
-- (a,Int) -> (b,Int)

runningSum :: [Int] -> Counter [Int]
runningSum [] = pure []
runningSum (x:xs) = do 
  s <- get 
  let s' = x + s
  put s'
  ys <- runningSum xs
  pure $ s':ys


  
--
-- 4. **Implementacja własnej monady**  
--
--    Zaimplementuj własną monadę `Logger a`, która będzie przechowywać wartość typu `a` wraz z logiem 
--    operacji (listą napisów). Zdefiniuj instancje `Functor`, `Applicative` i `Monad` dla tego typu.
--    Napisz funkcje pomocnicze:
--    - `logMessage :: String -> Logger ()`
--    - `runLogger :: Logger a -> (a, [String])`
--    Następnie użyj tej monady do zaimplementowania funkcji `factorial :: Int -> Logger Int`, 
--    która oblicza silnię i loguje każdy krok obliczeń.
--
-- 5. **Monada Writer do akumulacji wyników**  
--
--    Zaimplementuj funkcję `countNodes :: Tree a -> Writer (Sum Int) ()`, która liczy liczbę węzłów 
--    w drzewie binarnym, używając monady Writer do akumulacji sumy. Typ drzewa zdefiniuj jako 
--    `data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)` oraz do rozwiazania uzyj funkcji `tell`. 
--
data Tree a = Empty | Leaf a | Node a (Tree a) (Tree a)
countNodes :: Tree a -> Writer (Sum Int) () 
countNodes Empty = pure ()  
countNodes (Leaf _) = do 
  tell (Sum 1)
  pure ()
countNodes (Node _ left right) = do 
  tell (Sum 1)
  countNodes left 
  countNodes right
  pure () 


exampleTree = Node 3 (Leaf 5) (Leaf 6) 
