# Zadania Domowe z Haskella

## 1. Implementacja funkcji fold dla drzewa
Zdefiniuj typ drzewa binarnego:
```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
```
Następnie zaimplementuj funkcję:
```haskell
foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
```
która działa podobnie jak `foldr` dla list, ale dla drzew. Użyj tej funkcji do implementacji funkcji:
- `sumTree :: Num a => Tree a -> a` - suma wszystkich wartości w drzewie
- `heightTree :: Tree a -> Int` - wysokość drzewa
- `treeToList :: Tree a -> [a]` - konwersja drzewa do listy (inorder)

## 2. Rekurencja ogonowa w sortowaniu przez scalanie
Zaimplementuj funkcję `tailMergeSort :: Ord a => [a] -> [a]` wykorzystującą algorytm sortowania przez 
scalanie (merge sort), ale z użyciem rekurencji ogonowej. Upewnij się, że unikasz głębokiej rekurencji używając akumulatora.

## 3. Kalkulator wyrażeń
Zdefiniuj typ danych dla wyrażeń arytmetycznych:
```haskell
data Expression = Number Double 
               | Variable String 
               | Add Expression Expression 
               | Subtract Expression Expression 
               | Multiply Expression Expression 
               | Divide Expression Expression
```
Zaimplementuj funkcje:
- `evaluate :: [(String, Double)] -> Expression -> Double` - oblicza wartość wyrażenia przy danych wartościach zmiennych,
- `differentiate :: Expression -> String -> Expression` - różniczkuje wyrażenie po nazwie zmiennej z drugiego argumentu, 

## 4. Generator liczb Fibonacciego z rekurencją ogonkową
Napisz funkcję `fibTR :: Int -> Integer`, która oblicza n-tą liczbę Fibonacciego używając rekurencji ogonowej. 


## 5. Implementacja [kopca binarnego](https://en.wikipedia.org/wiki/Binary_heap)
Zdefiniuj typ danych dla kopca binarnego (min-heap):
```haskell
data Heap a = Empty | Node a (Heap a) (Heap a)
```
Zaimplementuj następujące operacje:
- `emptyHeap :: Heap a` - tworzy pusty kopiec
- `insertHeap :: Ord a => a -> Heap a -> Heap a` - dodaje element do kopca
- `findMinHeap :: Heap a -> Maybe a` - zwraca najmniejszy element z kopca
- `deleteMinHeap :: Ord a => Heap a -> Heap a` - usuwa najmniejszy element z kopca
- `heapify :: Ord a => [a] -> Heap a` - tworzy kopiec z listy elementów
- `heapSort :: Ord a => [a] -> [a]` - sortuje listę używając kopca

## 6. Rozwijanie i zwijanie danych
Zaimplementuj parę funkcji:
```haskell
unfold :: (b -> Maybe (a, b)) -> b -> [a]
fold :: (a -> b -> b) -> b -> [a] -> b
```
gdzie `unfold` generuje listę z początkowego stanu, a `fold` jest analogiczna do `foldl`. 
Następnie użyj tych funkcji do implementacji:
- `fibonacciSequence :: Int -> [Integer]` - generuje n pierwszych liczb Fibonacciego
- `convertBinaryToDecimal :: [Int] -> Int` - konwertuje liczbę binarną (reprezentowaną jako lista bitów) na liczbę dziesiętną


## 7. Graf i algorytmy grafowe
Zdefiniuj typ reprezentujący graf:
```haskell
data Graph a = Graph [(a, [a])]
```
gdzie każdy wierzchołek jest mapowany na listę sąsiadów. Zaimplementuj następujące funkcje:
- `dfs :: Eq a => a -> Graph a -> [a]` - przeszukiwanie grafu w głąb (DFS) zaczynając od danego wierzchołka
- `bfs :: Eq a => a -> Graph a -> [a]` - przeszukiwanie grafu wszerz (BFS) zaczynając od danego wierzchołka
- `hasPath :: Eq a => a -> a -> Graph a -> Bool` - sprawdza czy istnieje ścieżka pomiędzy dwoma wierzchołkami
