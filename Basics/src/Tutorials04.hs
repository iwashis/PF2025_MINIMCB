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

exampleTree2 :: Tree String 
exampleTree2 = Node "tomek" [ Node "ala" [], Node "maciek" [Node "ola" []]]


instance Functor Tree where 
-- fmap :: (a -> b) -> Tree a -> Tree b
 fmap f (Node x list) = Node (f x) (map (fmap f) list)

instance Foldable Tree where
-- ( a -> b -> b ) -> b -> Tree a -> b
  foldr f seed (Node x list) = f x (foldr (\a b ->  foldr f b a) seed list)
  -- (a -> m) -> Tree a -> m
  foldMap f (Node x list) = f x M.<>  (foldr (<>) mempty $ fmap (foldMap f) list)
  
--    Napisz instancje `Functor` oraz `Foldable` dla typu `Tree a` a nastepnie użyj ich do implementacji:
--    - `treeSum :: Num a => Tree a -> a` - sumuje wszystkie wartości w drzewie
--    - `treeDepth :: Tree a -> Int` - znajduje głębokość drzewa
--    - `treeToList :: Tree a -> [a]` - konwertuje drzewo do listy (w porządku pre-order)

treeSum :: Num a => Tree a -> a
treeSum tree = foldr (+) 0 tree


-- teraz napiszemy to samo uzywajac foldMap i monoidu w uproszczonej wersji
-- treeSum' :: Tree Int -> Int
instance Semigroup Int where 
  x <> y = x + y 

instance Monoid Int where 
  mempty = 0

treeSum' :: Tree Int -> Int 
treeSum' tree = foldMap id tree

-- treeToList 
treeToList' :: Tree a -> [a]
treeToList' tree = foldMap return tree -- return = \x -> [x]

-- 3. **Fold z kontrolą akumulacji**
--
-- Zaimplementuj funkcję `foldlWithControl :: (b -> a -> Either b c) -> b -> [a] -> Either b c`, która
-- działa jak `foldl`, ale pozwala na przerwanie obliczenia w dowolnym momencie, zwracając aktualny akumulator
-- opakowany w `Left` lub finalny wynik w `Right`. Następnie użyj tej funkcji do implementacji:
-- - `findFirstThat :: (a -> Bool) -> [a] -> Maybe a` - znajduje pierwszy element spełniający warunek
-- - `takeWhileSum :: (Num a, Ord a) => a -> [a] -> [a]` - zwraca najdłuższy prefiks listy, którego suma nie przekracza podanej wartości
-- - `findSequence :: Eq a => [a] -> [a] -> Maybe Int` - znajduje indeks pierwszego wystąpienia podlisty w liście


-- data Either a b = Left a | Right b
foldWithControl :: (b -> a -> Either b c) -> b -> [a] -> Either b c 
foldWithControl _ seed [] =  Left seed
foldWithControl f seed (x:xs) = case f seed x of 
  Left b  -> foldWithControl f b xs 
  Right c -> Right c 

-- () typ unit, ktory ma jeden konstruktor, ()
-- () :: ()
-- Maybe a = Just a | Nothing 
-- Either () a = Left () | Right a 
--
toMaybe :: Either () a -> Maybe a 
toMaybe (Left ()) = Nothing
toMaybe (Right x) = Just x

findFirstThat :: (a -> Bool) -> [a] -> Maybe a
findFirstThat f list = toMaybe $ foldWithControl g () list 
  where 
    -- g :: () -> a -> Either () a
    g () x = if f x  then Right x else Left ()

takeWhileSum :: Int -> [Int] -> Either [Int] [Int]
takeWhileSum max list = foldWithControl f [] list   -- takeWhileSum 10 [1,2,8,11] = Opakowane [1,2] 
  where 
  --  f :: (b -> a -> Either b c)
    f acc x = if sum acc + x < max then Left $ acc ++ [x] else Right acc 

-- 4. **Odwracanie foldów**
--
--    Zaimplementuj funkcję `unfoldl :: (b -> Maybe (b, a)) -> b -> [a]`, Użyj jej do implementacji:
--    - `countdown :: Int -> [Int]` - generuje odliczanie od n do 1
--    - `fib :: Int -> [Int]` - generuje n pierwszych liczb Fibonacciego
--    - `iterate' :: (a -> a) -> a -> [a]` - własna implementacja standardowej funkcji `iterate`
--    - `decToBin :: Int -> [Int]` - konwertuje liczbę dziesiętną na binarną reprezentację (listę 0 i 1)

unfold :: (b -> Maybe (b,a)) -> b -> [a]
unfold f s = go s [] 
  where 
    go state acc = case f state of 
      Nothing -> acc
      Just (state',obs) -> go state' (acc ++ [obs]) 

simpleAutomaton :: Char -> Maybe (Char, Int)
simpleAutomaton 'x' = Just ('y',1)
simpleAutomaton 'y' = Just ('z', 0)
simpleAutomaton 'z' = Nothing
simpleAutomaton _ = Nothing
-- sprawdzic czym  jest unfold simpleAutomaton 'x'

fib' :: Int -> [Int] 
fib' n = unfold f state 
  where 
    f :: (Int,Int,Int) -> Maybe ((Int,Int,Int),Int)
    f (_,_,0) = Nothing
    f (x,y,m) = Just ((y, x + y,m-1), x)  

    state = (0,1,n)

-- 5. **Zaawansowana transformacja danych**  
--
--    Napisz funkcję `foldTransform :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c`, która łączy 
--    dwie listy, stosując do nich funkcję trójargumentową i akumulator. Użyj jej do implementacji:
--    - `zipFoldl :: (c -> a -> b -> c) -> c -> [a] -> [b] -> c` - podobne do `zipWith`, ale z akumulacją
--    - `matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]` - mnożenie macierzy przy użyciu foldów
--
