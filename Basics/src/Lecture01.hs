module Lecture01 where


-- Implementacja algorytmu quicksort w Haskellu
-- Algorytm działa rekurencyjnie:
-- 1. Jeśli lista jest pusta, zwracamy pustą listę (przypadek bazowy)
-- 2. W przeciwnym razie bierzemy pierwszy element jako pivot
-- 3. Dzielimy pozostałe elementy na mniejsze i większe/równe od pivota
-- 4. Sortujemy rekurencyjnie obie podlisty i łączymy wyniki
-- 
-- Przykłady:
-- quicksort [3,1,4,1,5,9,2,6] = [1,1,2,3,4,5,6,9]
-- quicksort "haskell" = "aehklls"
-- quicksort [] = []
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = quicksort le ++ [x] ++ quicksort gr
  where
    le = filter (< x) xs     -- elementy mniejsze od pivota
    gr = filter (>= x) xs    -- elementy większe lub równe pivotowi

-- Funkcja curried dodająca dwie liczby
-- add przyjmuje argument x i zwraca funkcję, która przyjmuje argument y
-- i zwraca sumę x + y
-- 
-- Przykłady:
-- add 2 3 = 5
-- (add 2) 3 = 5
-- 
-- W Haskellu wszystkie funkcje są domyślnie curried,
-- więc add x y = x + y jest równoważne poniższej definicji
add :: Int -> (Int -> Int)
add x = \y -> x + y
-- add x y =  x + y

-- Częściowa aplikacja funkcji add - tworzymy nową funkcję, która dodaje 6
-- do swojego argumentu
-- 
-- Przykłady:
-- t 4 = 10
-- t 0 = 6
-- map t [1,2,3] = [7,8,9]
t = add 6

-- Nieskończona lista jedynek
-- Haskell dzięki leniwej ewaluacji może pracować z nieskończonymi strukturami
-- 
-- Przykłady:
-- take 5 ones = [1,1,1,1,1]
-- sum (take 100 ones) = 100
ones = 1 : ones

-- Ciąg Fibonacciego zdefiniowany za pomocą list składających (list comprehension)
-- i rekurencji
-- 
-- Przykłady:
-- take 10 fib = [0,1,1,2,3,5,8,13,21,34]
-- fib !! 6 = 8
fib = 0 : 1 : [x + y | (x, y) <- zip fib (drop 1 fib)]

-- Prosta funkcja sumująca elementy listy
-- Implementacja rekurencyjna:
-- 1. Suma pustej listy to 0 (przypadek bazowy)
-- 2. Suma niepustej listy to głowa listy plus suma ogona
-- 
-- Przykłady:
-- sum' [] = 0
-- sum' [1,2,3,4,5] = 15
-- sum' [-3,5,10] = 12
sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs

