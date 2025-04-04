module Tutorials02 () where

-- # Rekurencja ogonkowa
--
-- 1. **Rekurencja ogonkowa i NWD z rozszerzonym przetwarzaniem danych wejściowych**
--     Napisz funkcję `tailGCD :: Integral a => a -> a -> a`, która oblicza największy
--     wspólny dzielnik (NWD) dwóch liczb całkowitych.

tailGCD :: (Integral a) => a -> a -> a
tailGCD a 0 = a
tailGCD a b = tailGCD b (mod a b)

--
-- 2. **Rekurencja ogonkowa i quicksort z użyciem jawnego stosu**
--     Zaimplementuj wersję algorytmu quicksort: `tailQuickSort :: Ord a => [a] -> [a]`,
--     która unika głębokiej rekurencji, używając akumulatora lub jawnego stosu do
--     zarządzania podlistami, które trzeba posortować.

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = quickSort left ++ [x] ++ quickSort right
  where
    left = filter (<= x) xs
    right = filter (> x) xs

data ActionState a b = Completed a | Pending b -- izomorficzne z Either a b

quickSortTail :: (Ord a) => [a] -> [a]
quickSortTail list = go [Pending list] []
  where
    go [] xs = xs
    go ((Completed a) : stack) xs = go stack (a : xs)
    go ((Pending []) : stack) xs = go stack xs
    go ((Pending [a]) : stack) xs = go (Completed a : stack) xs
    go ((Pending (a : as)) : stack) xs = go (Pending right : Completed a : Pending left : stack) xs
      where
        left = [b | b <- as, b <= a]
        right = [b | b <- as, b > a]

-- 3. **Rekurencja ogonkowa i obliczanie zbioru potęgowego (power set)**
--     Napisz funkcję `tailPowerSet :: [a] -> [[a]]`, która oblicza zbiór potęgowy
--     (power set) danej listy, wykorzystując tail recursion.
--     Upewnij się, że:
--     - Używasz akumulatora, który stopniowo buduje zbiór potęgowy.
--     - Unikasz tworzenia się pośrednich wyrażeń (thunks) podczas łączenia podzbiorów.
--     - Funkcja działa efektywnie nawet dla list o umiarkowanym rozmiarze.

powerSet :: [a] -> [[a]]
powerSet ls = go ls [[]]
  where
    go :: [a] -> [[a]] -> [[a]]
    go [] acc = acc
    go (x : xs) acc = go xs (map (x :) acc ++ acc)

-- [1,2,3]
-- [1,2,3] [[]]
-- [2,3] [[1],[]]
-- [3] [[2,1],[2],[1],[]]
-- [] [ [3,2,1],[3,2],[3,1],[3],[2,1],[2],[1],[]]

-- 4. **Rekurencja ogonkowa i sumowanie zagnieżdżonej struktury list**
--     Zdefiniuj typ rekurencyjny dla zagnieżdżonych list:
--     ```haskell
--     data NestedList a = Elem a | List [NestedList a]
--     ```
--     Następnie napisz tail-recursive funkcję:
--     `sumNested :: Num a => NestedList a -> a`,
--     która oblicza sumę wszystkich elementów w zagnieżdżonej liście.

data NestedList a = Elem a | List [NestedList a]
    deriving (Show)

example1 :: NestedList Int
example1 = Elem 4
example2 :: NestedList Int
example2 = List [Elem 5, List [Elem 5, Elem 7]]

sumNested :: (Num a) => NestedList a -> a
sumNested nestedList = go [nestedList] 0
  where
    go [] n = n
    go ((Elem x) : xs) n = go xs (x + n)
    go ((List x) : xs) n = go (x ++ xs) n

-- [ List [Elem 5, List [Elem 5, Elem 7]] ] 0
-- [Elem 5, List [Elem 5, Elem 7]] 0
-- [List [Elem 5, Elem 7]] 5
-- [Elem 5, Elem 7], 5
-- [Elem 7] 10
-- [] 17

-- 5. **Rekurencja ogonkowa i przeglądanie drzewa**
--     Dla drzewa binarnego zdefiniowanego jako:
--     ```haskell
--     data Tree a = Empty | Node a (Tree a) (Tree a)
--     ```
--     napisz funkcję: `preorder :: Tree a -> [a]`,
--     która odwiedza węzły drzewa w następującej kolejności:
--     najpierw bieżący węzeł, potem jego lewe poddrzewo, a na końcu prawe poddrzewo, zwracając listę wartości w tej kolejności.List
--

data Tree a = Empty | Node a (Tree a) (Tree a)

exampleTree2 :: Tree Int
exampleTree2 =
    Node
        3
        (Node 5 Empty (Node 7 Empty Empty))
        (Node 20 (Node 13 Empty Empty) Empty)

preorder :: Tree a -> [a]
preorder tree = go [Pending tree] []
  where
    go [] xs = xs
    go ((Completed a) : stack) xs = go stack (a : xs)
    go ((Pending Empty) : stack) xs = go stack xs
    go ((Pending (Node x left right)) : stack) xs = go (Pending right : Pending left : Completed x : stack) xs
