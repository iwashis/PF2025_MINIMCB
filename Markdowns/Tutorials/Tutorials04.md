# Foldables 

1. **Implementacja map i filter za pomocą foldów**  

   Zaimplementuj funkcje `myMap :: (a -> b) -> [a] -> [b]` oraz `myFilter :: (a -> Bool) -> [a] -> [a]` 
   używając zarówno `foldr` jak i `foldl`. Porównaj ich działanie i wydajność w kontekście leniwej ewaluacji. 

2. **Niestandardowy fold dla drzew**  

   Zdefiniuj typ danych dla drzewa wielokierunkowego:
   ```haskell
   data Tree a = Node a [Tree a]
   ```

   Napisz instancje `Foldable` dla typu `Tree a` a nastepnie użyj jej do implementacji:
   - `treeSum :: Num a => Tree a -> a` - sumuje wszystkie wartości w drzewie
   - `treeDepth :: Tree a -> Int` - znajduje głębokość drzewa
   - `treeToList :: Tree a -> [a]` - konwertuje drzewo do listy (w porządku pre-order)
   - `treeMap :: (a -> b) -> Tree a -> Tree b` - aplikuje funkcję do każdego węzła drzewa


3. **Fold z kontrolą akumulacji**  

   Zaimplementuj funkcję `foldlWithControl :: (b -> a -> Either b c) -> b -> [a] -> Either b c`, która 
   działa jak `foldl`, ale pozwala na przerwanie obliczenia w dowolnym momencie, zwracając aktualny akumulator 
   opakowany w `Left` lub finalny wynik w `Right`. Następnie użyj tej funkcji do implementacji:
   - `findFirstThat :: (a -> Bool) -> [a] -> Maybe a` - znajduje pierwszy element spełniający warunek
   - `takeWhileSum :: (Num a, Ord a) => a -> [a] -> [a]` - zwraca najdłuższy prefiks listy, którego suma nie przekracza podanej wartości
   - `findSequence :: Eq a => [a] -> [a] -> Maybe Int` - znajduje indeks pierwszego wystąpienia podlisty w liście

4. **Odwracanie foldów**  

   Zaimplementuj funkcję `unfoldl :: (b -> Maybe (b, a)) -> b -> [a]`, która jest odwrotnością `foldl` - 
   generuje listę z początkowego stanu. Użyj jej do implementacji:
   - `countdown :: Int -> [Int]` - generuje odliczanie od n do 1
   - `fib :: Int -> [Int]` - generuje n pierwszych liczb Fibonacciego
   - `iterate' :: (a -> a) -> a -> [a]` - własna implementacja standardowej funkcji `iterate`
   - `decToBin :: Int -> [Int]` - konwertuje liczbę dziesiętną na binarną reprezentację (listę 0 i 1)

6. **Zaawansowana transformacja danych**  

   Napisz funkcję `foldTransform :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c`, która łączy 
   dwie listy, stosując do nich funkcję trójargumentową i akumulator. Użyj jej do implementacji:
   - `zipFoldl :: (c -> a -> b -> c) -> c -> [a] -> [b] -> c` - podobne do `zipWith`, ale z akumulacją
   - `matrixMultiply :: Num a => [[a]] -> [[a]] -> [[a]]` - mnożenie macierzy przy użyciu foldów

7. **Fold dla złożonych struktur danych**  

   Zdefiniuj własną strukturę grafu:
   ```haskell
   data Graph a = Graph [(a, [(a, Int)])]  -- (węzeł, lista sąsiadów z wagami krawędzi)
   ```
   Zaimplementuj instacje `Foldable` dla `Graph a`.  Użyj jej do implementacji:
   - `findShortestPath :: Ord a => Graph a -> a -> a -> Maybe ([a], Int)` - znajduje najkrótszą ścieżkę 
     między dwoma węzłami (algorytm Dijkstry)
   - `isConnected :: Ord a => Graph a -> Bool` - sprawdza, czy graf jest spójny

8. **Uniwersalna funkcja fold**  

   Napisz funkcję `generalFold :: (a -> Either b c) -> ([c] -> b) -> [a] -> b`, która łączy elementy listy 
   w bardziej złożony sposób:
   - Każdy element jest transformowany przez pierwszą funkcję
   - Jeśli wynik jest `Left b`, ten element kończy akumulację i zwraca b
   - Jeśli wynik jest `Right c`, c jest zbierane do listy
   - Na końcu druga funkcja jest aplikowana do zebranej listy

   Następnie użyj `generalFold` do implementacji:
   - `takeUntil :: (a -> Bool) -> [a] -> [a]` - zbiera elementy aż do spełnienia predykatu
   - `groupBySum :: (Num a, Ord a) => a -> [a] -> [[a]]` - grupuje elementy listy tak, aby suma każdej grupy
     nie przekraczała zadanej wartości
