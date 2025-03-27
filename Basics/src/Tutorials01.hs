{-# LANGUAGE BangPatterns #-}

module Tutorials01 () where

-- # List comprehensions
--
-- 1. **Trójki Pitagorejskie**
--    Napisz funkcję `pythagoreanTriples :: Int -> [(Int, Int, Int)]`, która zwraca wszystkie trójki `(a, b, c)` spełniające warunki:
--    - `1 ≤ a < b < c ≤ n`
--    - `a² + b² == c²`
--    Wykorzystaj list comprehensions do wygenerowania wyniku.
pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n =
    [ (a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], a < b, b < c, a ^ 2 + b ^ 2 == c ^ 2
    ]

-- 2. **Pary liczb, których suma jest liczbą pierwszą**
--    Napisz funkcję `primeSumPairs :: [Int] -> [(Int, Int)]`, która przyjmuje listę liczb
--    całkowitych i zwraca wszystkie unikalne pary `(x, y)` (przy założeniu, że `x < y`),
--    dla których suma `x + y` jest liczbą pierwszą.

isPrime :: Int -> Bool
isPrime n = length (filter (\x -> mod n x == 0) [1 .. n]) == 2
primeSumPairs list = [(a, b) | a <- list, b <- list, a < b, isPrime $ a + b]

-- 3. **Wyodrębnianie podłańcuchów**
--    Napisz funkcję `substrings :: String -> [String]`, która zwraca listę wszystkich niepustych
--    podłańcuchów danego ciągu znaków. Na przykład dla ciągu `"abc"` wynik powinien
--    zawierać `"a"`, `"ab"`, `"abc"`, `"b"`, `"bc"` oraz `"c"`. Wykorzystaj
--    list comprehensions do wygenerowania wszystkich podłańcuchów.

substrings :: String -> [String]
substrings string =
    [ take l (drop s string) | s <- [0 .. length string - 1], l <- [1 .. length string - s]
    ]

-- 4. **Pary dzielników**
--    Napisz funkcję `divisorPairs :: [Int] -> [(Int, Int)]`, która
--    przyjmuje listę liczb całkowitych i zwraca wszystkie różne pary `(x, y)`
--    (przy założeniu, że `x ≠ y`), dla których `x` dzieli `y` bez reszty (tj. `y mod x == 0`).

divisorPairs :: [Int] -> [(Int, Int)]
divisorPairs list = [(x, y) | x <- list, y <- list, x /= y, y `mod` x == 0]

-- 5. **Kombinacje**
--     Napisz funkcję
--     ```haskell
--     combinations :: Int -> [a] -> [[a]]
--     ```
--     która generuje wszystkie kombinacje k-elementowe z danej listy.
--     Na przykład, dla `k = 2` i listy `[1,2,3]` wynikiem powinno być `[[1,2], [1,3], [2,3]]`.

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations k (x : xs) = [x : comb | comb <- combinations (k - 1) xs] ++ combinations k xs

--
-- # Leniwa/gorliwa ewaluacja, `seq` i bang patterns
--
-- 6. **Ścisła suma z użyciem `seq`**
--    Napisz funkcję `strictSum :: [Int] -> Int`, która oblicza sumę listy liczb całkowitych,
--    używając `seq` do wymuszenia ewaluacji akumulatora na każdym kroku.
--    Porównaj jej działanie z naiwną, leniwą implementacją sumowania.

sumLazyNonTail :: [Int] -> Int
sumLazyNonTail [] = 0
sumLazyNonTail (x : xs) = x + sumLazyNonTail xs

sumLazy :: [Int] -> Int
sumLazy list = go list 0
  where
    go :: [Int] -> Int -> Int
    go [] s = s
    go (x : xs) s = go xs (x + s)

sum :: [Int] -> Int
sum list = go list 0
  where
    go :: [Int] -> Int -> Int
    go [] !s = s
    go (x : xs) !s = go xs (x + s)

sumSeq :: [Int] -> Int
sumSeq list = go list 0
  where
    go :: [Int] -> Int -> Int
    go [] s = s
    go (x : xs) s = let s' = s `seq` x + s in go xs s'

-- 7. **Rekurencyjna funkcja silnia z użyciem bang patterns**
--    Napisz rekurencyjną funkcję `factorial :: Int -> Int`, która oblicza silnię danej liczby.
--    Użyj bang patterns w akumulatorze.

factorialNaive :: Int -> Int
factorialNaive 0 = 1
factorialNaive !n = n * factorialNaive (n - 1)

factorial n = go n 1
  where
    go 0 acc = acc
    go n !acc = go (n - 1) (n * acc)

-- 8. **Wymuszanie ewaluacji elementów krotki**
--    Napisz funkcję `forceTuple :: (Int, Int) -> Int`, która przyjmuje krotkę dwóch liczb całkowitych,
--    wymusza ewaluację obu elementów za pomocą `seq`, a następnie zwraca ich sumę.
--    Wyjaśnij, dlaczego wymuszanie ewaluacji może być konieczne w niektórych sytuacjach.
--
-- 9. **Liczby Fibonacciego z `seq` vs. bang patterns**
--    Zaimplementuj dwie wersje generatora liczb Fibonacciego:
--    - Pierwsza wersja wykorzystuje `seq` do wymuszenia ewaluacji w funkcji pomocniczej.
--    - Druga wersja używa bang patterns w argumentach funkcji pomocniczej.
--
-- 10. **Unikanie wycieków pamięci w funkcji rekurencyjnej**
--     Napisz funkcję `strictRecursive :: Int -> Int`, która oblicza wynik przy użyciu rekurencji,
--     gdzie leniwa ewaluacja mogłaby prowadzić do wycieków pamięci.
--     Zrefaktoryzuj funkcję, używając `seq` lub bang patterns, aby wymusić ścisłą ewaluację.
--
--
--
