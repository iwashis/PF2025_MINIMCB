module Lecture03 () where
import qualified Data.Monoid as M

-- Przypomnienie działania funkcji foldl i foldr:
-- foldl (#) seed [a1..an] -> ((..(seed#a1)#a2#..)#an
-- foldr (*) seed [a1..an] -> a1*(a2*..(an*seed))..)
--
-- foldl :: (b -> a -> b) -> b -> [a] -> b

-- Funkcja initl tworzy listę wszystkich początkowych podlist
-- Przykład: initl [1,2,3] = [[], [1], [1,2], [1,2,3]]
-- Używa foldl do budowania wyniku krok po kroku
initl :: [a] -> [[a]]
initl = foldl f seed 
  where
    seed = [[]]  -- wartość początkowa to lista zawierająca pustą listę
    f s c = s ++ [last s ++ [c]]  -- dodajemy nowy element do ostatniej podlisty i dołączamy do wyniku

-- Przykłady użycia initl:
-- initl "abc" = ["", "a", "ab", "abc"]
-- initl [10,20,30] = [[], [10], [10,20], [10,20,30]]

-- Teraz napiszemy init używając foldr
-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr (*) seed [a1..an] -> a1*(a2*..(an*seed))..)
--
initr :: [a] -> [[a]]
initr = foldr f seed 
  where
    seed = [[]]
    f x acc = [] : map (x:) acc


-- Suma listy
sumList :: [Int] -> Int
sumList = foldl (+) 0

-- Przykład: sumList [1,2,3,4,5] = ((((0+1)+2)+3)+4)+5 = 15

-- Odwrócenie listy
reverseList :: [a] -> [a]
reverseList = foldl (\acc x -> x : acc) []

-- Przykład: reverseList [1,2,3] = (([] ++ [1]) ++ [2]) ++ [3] = [3,2,1]
-- Krok po kroku:
-- 1. acc = [], x = 1 => x : acc = [1]
-- 2. acc = [1], x = 2 => x : acc = [2,1]
-- 3. acc = [2,1], x = 3 => x : acc = [3,2,1]

-- Zliczanie wystąpień elementu
countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences y = foldl (\acc x -> if x == y then acc + 1 else acc) 0

-- Przykład: countOccurrences 'a' "abracadabra" = 5

-- 
-- Foldy na innych typach danych
--
-- Definicja drzewa binarnego
data Tree a = EmptyTree | Leaf a | Node a (Tree a) (Tree a)

-- Przykładowe drzewo z napisami
tree :: Tree String
tree = Node "a" (Node "b" EmptyTree (Leaf "c")) (Node "d" EmptyTree EmptyTree)

-- Przykładowe drzewo z liczbami całkowitymi
tree2 :: Tree Int
tree2 = Node 1 (Node 5 EmptyTree (Leaf 7)) (Node 3 EmptyTree EmptyTree)

-- Implementacja interfejsu Foldable dla drzewa
-- Pozwala na używanie funkcji fold, foldMap, foldr, foldl, itd. na drzewach
instance Foldable Tree where
    -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
    -- Mapuje każdy element drzewa na monoid i łączy wyniki
    foldMap _ EmptyTree = M.mempty  -- puste drzewo to element neutralny monoidu
    foldMap f (Leaf x) = f x  -- liść to po prostu wartość przekształcona przez f
    foldMap f (Node x left right) = foldMap f left `M.mappend` f x `M.mappend` foldMap f right
    -- Przechodzenie drzewa w porządku inorder: lewe poddrzewo, węzeł, prawe poddrzewo

    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    -- Składa drzewo "od prawej"
    foldr _ seed EmptyTree = seed  -- puste drzewo zwraca wartość początkową
    foldr f seed (Leaf x) = f x seed  -- liść aplikuje funkcję f do swojej wartości i seed
    foldr f seed (Node x left right) = foldr f (f x (foldr f seed right)) left
    -- Kolejność obliczania: najpierw prawe poddrzewo, potem węzeł, potem lewe poddrzewo


-- definiujemy typ pomocniczy: 
newtype Any = Any { getAny :: Bool }
-- oraz przedstawiamy definicje instancji monoid (+semigroup),  zeby uzyc foldMap
instance Semigroup Any where 
 (<>) (Any x) (Any y) = Any (x || y)

instance Monoid Any where
  mempty = Any False 


-- Sprawdzanie, czy drzewo zawiera określoną wartość
treeContains :: Eq a => a -> Tree a -> Bool
treeContains x = getAny . foldMap (\y -> Any (y == x))
-- Przykład: treeContains 7 tree2 = True
-- Przykład: treeContains 10 tree2 = False

-- Zbieranie wszystkich wartości z drzewa do listy (inorder)
treeToList :: Tree a -> [a]
treeToList = foldMap (\x -> [x])

-- Przykład: treeToList tree = ["b", "c", "a", "d"]
-- Przykład: treeToList tree2 = [5, 7, 1, 3]
