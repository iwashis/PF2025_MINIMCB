module Tutorials02 where

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



data Do a b = Done a | Undone b  -- izomorficzne z Either a b

quickSortTail :: (Ord a) => [a] -> [a]
quickSortTail list = go [Undone list] []
  where
    go [] xs = xs
    go ((Done a) : stack) xs = go stack (a : xs)
    go ((Undone []) : stack) xs = go stack xs
    go ((Undone [a]) : stack) xs = go (Done a : stack) xs
    go ((Undone (a : as)) : stack) xs = go (Undone right : Done a : Undone left : stack) xs
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
preorder tree = go [Undone tree] []
  where 
    go [] xs = xs 
    go ((Done a):stack) xs = go stack (a : xs)
    go ((Undone Empty):stack) xs = go stack xs 
    go ((Undone (Node x left right)):stack) xs = go (Undone right : Undone left : Done x  : stack) xs 

-- # ADT i typeclassy
--
-- 6. **Słownik oparty na drzewie binarnym z balansowaniem**
--     Zdefiniuj algebryczny typ danych reprezentujący drzewo wyszukiwań binarnych (BST),
--     które będzie służyło jako słownik mapujący klucze na wartości. Następnie zaimplementuj
--     następujące operacje:
--     - *Wstawianie*: Dodanie pary klucz-wartość.
--     - *Wyszukiwanie*: Pobranie wartości przypisanej do klucza.
--     - *Usuwanie*: Usunięcie klucza (oraz odpowiadającej mu wartości) z drzewa.
--     - *Aktualizacja*: Modyfikacja wartości przypisanej do klucza.
--     - *Balansowanie*: Zaimplementuj procedurę balansowania (np. wykorzystując algorytm drzewa
--         AVL lub czerwono-czarnego), aby drzewo pozostało zbalansowane po operacjach wstawiania i usuwania.
--
-- 7. **Interpreter wyrażeń z różniczkowaniem i upraszczaniem**
--     Zdefiniuj algebraiczny typ danych reprezentujący wyrażenia arytmetyczne
--     (uwzględniający zmienne, stałe, dodawanie, mnożenie i potęgowanie). Napisz funkcje, które:
--     - *Ewaluacja*: Obliczają wartość numeryczną wyrażenia, korzystając z mapowania zmiennych na liczby.
--     - *Różniczkowanie*: Symbolicznie różniczkują wyrażenie względem danej zmiennej.
--     - *Upraszczanie*: Redukują wyrażenie do prostszej formy poprzez stosowanie uproszczeń algebraicznych
--     (np. eliminowanie składników zerowych, łączenie wyrazów podobnych).
--
-- 8. **Własna leniwa lista z obsługą nieskończoności**
--     Stwórz własny typ listy (np. `data MyList a = Nil | Cons a (MyList a)`), który wspiera leniwą ewaluację.
--     Zaimplementuj następujące funkcje:
--     - `myMap`: Funkcję analogiczną do `map`.
--     - `myFoldr`: Funkcję złożenia prawego (`foldr`), która potrafi działać na nieskończonych listach, jeśli to możliwe.
--     - `myFilter`: Funkcję analogiczną do `filter`.
--
-- 9. **Reprezentacja grafu i algorytmy**
--     Zdefiniuj algebryczny typ danych reprezentujący graf nieskierowany, w którym wierzchołki mogą
--     przechowywać dowolne dane. Napisz funkcje, które:
--     - *Przeszukiwanie w głąb (DFS)*: Przemierzają graf, zaczynając od danego wierzchołka.
--     - *Wykrywanie cykli*: Sprawdzają, czy graf zawiera cykle.
--     - *Znajdowanie ścieżki*: Znajdują ścieżkę między dwoma wierzchołkami (jeśli taka istnieje).
--
-- 10. **Znane typeclassy dla drzew**
--     Zdefiniuj instancje `Show, Eq, Semigroup, Monoid, Foldable, Functor` dla parametrycznego typu danych
--     `data T a = EmptyT | LeafT a | InnerT (T a) (T a)`

data T a = EmptyT | LeafT a | InnerT (T a) (T a)

instance (Show a) => Show (T a) where
  show EmptyT = ""
  show (LeafT x) = show x 
  show (InnerT left right) = let 
      l = show left 
      r = show right 
    in l ++ (if r /= "" then ", " ++ r else "")


exampleTree3 :: T Int
exampleTree3 = InnerT (LeafT 10) (InnerT (LeafT 13) (LeafT 14))

