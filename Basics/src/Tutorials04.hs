module Tutorials04 where

import qualified Data.Monoid as M 
-- # Foldables
--
-- 1. **Implementacja map i filter za pomocą foldów**
--
--    Zaimplementuj funkcje `myMap :: (a -> b) -> [a] -> [b]` oraz `myFilter :: (a -> Bool) -> [a] -> [a]`
--    używając zarówno `foldr` jak i `foldl`. 
myMap' :: (a -> b) -> [a] -> [b]
-- foldl (*) seed [a1,..an] = (seed * a1) * a2 ... * an 
myMap' f list = foldl (\b a ->  b ++ [f a] ) [] list -- (b -> a -> b) -> b -> [a] -> b

-- Przyklad list = ["tomek", "ala"]
-- f = length
-- foldl (\b a ->  b ++ [f a] ) [] list = ([] * "tomek") * "ala"
-- [] * "tomek" = [] ++ [ f "tomek"] = [] ++ [5] = [5]
-- [5] * "ala"  = [5] ++ [ f "ala" ] = [5] ++ [3] = [5,3] 

myMapr :: (a -> b) -> [a] -> [b]
-- foldr (#) seed [a1,..an] = a1 # (a2 # ... (an # seed)) 
-- ( a -> b -> b ) -> b -> [a] -> b
myMapr f list = foldr (\a b -> (f a) : b ) [] list 
myFilter :: (a -> Bool) -> [a] -> [a]
-- foldl (*) seed [a1,..an] = (seed * a1) * a2 ... * an 
-- (b -> a -> b) -> b -> [a] -> b
myFilter f list = foldl g [] list  
  where 
    g b a  = if f a then b ++ [a] else b 

--TODO: napisac z foldr
--
-- 2. **Niestandardowy fold dla drzew**
--
--    Zdefiniuj typ danych dla drzewa:
--    ```haskell
--    data Tree a = Node a [Tree a]
--    ```

data Tree a = Node a [Tree a]
  deriving Show

exampleTree :: Tree Int 
exampleTree = Node 1 [ Node 2 [], Node 5 [Node 6 []]]

exampleTreee :: Tree String 
exampleTreee = Node "tomek" [ Node "ala" [], Node "maciek" [Node "ola" []]]


instance Functor Tree where 
-- fmap :: (a -> b) -> Tree a -> Tree b
 fmap f (Node x list) = Node (f x) (map (fmap f) list)

instance Foldable Tree where
-- foldr (#) seed [a1,..an] = a1 # (a2 # ... (an # seed)) 
-- ( a -> b -> b ) -> b -> Tree a -> b
  foldr f seed (Node x list) = f x (foldr (\a b ->  foldr f b a) seed list)
  -- (a -> m) -> Tree a -> m
  foldMap f (Node x list) = f x M.<>  (foldr (<>) mempty $ fmap (foldMap f) list)
  
--    Napisz instancje `Functor` oraz `Foldable` dla typu `Tree a` a nastepnie użyj ich do implementacji:
--    - `treeSum :: Num a => Tree a -> a` - sumuje wszystkie wartości w drzewie
--    - `treeDepth :: Tree a -> Int` - znajduje głębokość drzewa
--    - `treeToList :: Tree a -> [a]` - konwertuje drzewo do listy (w porządku pre-order)


-- TODO: dokonczyc
--
-- 3. **Fold z kontrolą akumulacji**
--
--    Zaimplementuj funkcję `foldlWithControl :: (b -> a -> Either b c) -> b -> [a] -> Either b c`, która
--    działa jak `foldl`, ale pozwala na przerwanie obliczenia w dowolnym momencie, zwracając aktualny akumulator
--    opakowany w `Left` lub finalny wynik w `Right`. Następnie użyj tej funkcji do implementacji:
--    - `findFirstThat :: (a -> Bool) -> [a] -> Maybe a` - znajduje pierwszy element spełniający warunek
--    - `takeWhileSum :: (Num a, Ord a) => a -> [a] -> [a]` - zwraca najdłuższy prefiks listy, którego suma nie przekracza podanej wartości
--    - `findSequence :: Eq a => [a] -> [a] -> Maybe Int` - znajduje indeks pierwszego wystąpienia podlisty w liście
--
-- 4. **Odwracanie foldów**
--
--    Zaimplementuj funkcję `unfoldl :: (b -> Maybe (b, a)) -> b -> [a]`, która jest odwrotnością `foldl` -
--    generuje listę z początkowego stanu. Użyj jej do implementacji:
--    - `countdown :: Int -> [Int]` - generuje odliczanie od n do 1
--    - `fib :: Int -> [Int]` - generuje n pierwszych liczb Fibonacciego
--    - `iterate' :: (a -> a) -> a -> [a]` - własna implementacja standardowej funkcji `iterate`
--    - `decToBin :: Int -> [Int]` - konwertuje liczbę dziesiętną na binarną reprezentację (listę 0 i 1)
--
-- 5. **Zaawansowana transformacja danych**  
--
--    Napisz funkcję `foldTransform :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c`, która łączy 
--    dwie listy, stosując do nich funkcję trójargumentową i akumulator. Użyj jej do implementacji:
--    - `zipFoldl :: (c -> a -> b -> c) -> c -> [a] -> [b] -> c` - podobne do `zipWith`, ale z akumulacją
--    - `matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]` - mnożenie macierzy przy użyciu foldów
--
-- 6. **Uniwersalna funkcja fold**  
--
--    Napisz funkcję `generalFold :: (a -> Either b c) -> ([c] -> b) -> [a] -> b`, która łączy elementy listy 
--    w bardziej złożony sposób:
--    - Każdy element jest transformowany przez pierwszą funkcję
--    - Jeśli wynik jest `Left b`, ten element kończy akumulację i zwraca b
--    - Jeśli wynik jest `Right c`, c jest zbierane do listy
--    - Na końcu druga funkcja jest aplikowana do zebranej listy
--
--    Następnie użyj `generalFold` do implementacji:
--    - `takeUntil :: (a -> Bool) -> [a] -> [a]` - zbiera elementy aż do spełnienia predykatu
--    - `groupBySum :: (Num a, Ord a) => a -> [a] -> [[a]]` - grupuje elementy listy tak, aby suma każdej grupy
--      nie przekraczała zadanej wartości
