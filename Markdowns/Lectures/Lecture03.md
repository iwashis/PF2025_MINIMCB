# Wykład 3 - Złożenia (Foldy) w Haskellu

## Podstawowe Złożenia: foldl i foldr

Na początek przypomnijmy sobie działanie dwóch podstawowych funkcji złożeń: `foldl` (złożenie lewostronnie) i `foldr` (złożenie prawostronnie).

```haskell
-- foldl (#) seed [a1..an] -> ((..(seed#a1)#a2#..)#an
-- foldr (*) seed [a1..an] -> a1*(a2*..(an*seed))..)
--
-- foldl :: (b -> a -> b) -> b -> [a] -> b
```

Funkcja `foldl` składa listę od lewej strony, zaczynając od wartości początkowej 
(seed) i aplikując funkcję kumulacyjnie do każdego elementu. Z kolei `foldr` składa listę od prawej strony.

## Przykłady Zastosowania Złożeń

### Tworzenie Listy Wszystkich Podlist Początkowych

Zobaczmy praktyczne zastosowanie złożeń na przykładzie funkcji `initl`, która tworzy listę wszystkich początkowych podlist:

```haskell
initl :: [a] -> [[a]]
initl = foldl f seed 
  where
    seed = [[]]  -- wartość początkowa to lista zawierająca pustą listę
    f s c = s ++ [last s ++ [c]]  -- dodajemy nowy element do ostatniej podlisty i dołączamy do wyniku
```

Funkcja `initl` używa `foldl` do budowania wyniku krok po kroku. 
Zaczynamy od listy zawierającej pustą listę, a następnie 
dla każdego nowego elementu tworzymy nową podlistę przez dodanie go do ostatniej utworzonej podlisty.

Przykłady:
```
> initl "abc"
["","a","ab","abc"]
> initl [10,20,30]
[[],[10],[10,20],[10,20,30]]
```

Możemy zaimplementować tę samą funkcję używając `foldr`:

```haskell
initr :: [a] -> [[a]]
initr = foldr f seed 
  where
    seed = [[]]
    f x acc = [] : map (x:) acc
```

Różnica polega na sposobie konstruowania wyniku - w `foldr` zaczynamy od końca listy i budujemy wynik od prawej strony.

### Suma Elementów Listy

Najprostszym przykładem zastosowania złożenia jest obliczanie sumy elementów listy:

```haskell
sumList :: [Int] -> Int
sumList = foldl (+) 0
```

Przykład:
```
> sumList [1,2,3,4,5]
15
```

W tym przypadku `foldl` działa tak: `((((0+1)+2)+3)+4)+5 = 15`

### Odwrócenie Listy

Kolejnym ciekawym przykładem jest odwrócenie listy:

```haskell
reverseList :: [a] -> [a]
reverseList = foldl (\acc x -> x : acc) []
```

Przykład:
```
> reverseList [1,2,3]
[3,2,1]
```

Krok po kroku:
1. `acc = [], x = 1 => x : acc = [1]`
2. `acc = [1], x = 2 => x : acc = [2,1]`
3. `acc = [2,1], x = 3 => x : acc = [3,2,1]`

### Zliczanie Wystąpień Elementu

Złożenia są również przydatne do analizy danych, np. zliczania wystąpień elementu:

```haskell
countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences y = foldl (\acc x -> if x == y then acc + 1 else acc) 0
```

Przykład:
```
> countOccurrences 'a' "abracadabra"
5
```

## Złożenia na Własnych Typach Danych

Haskell pozwala rozszerzyć koncepcję złożeń na własne typy danych 
poprzez implementację interfejsu `Foldable`. 
Zdefiniujmy drzewo binarne i zaimplementujmy dla niego `Foldable`:

```haskell
data Tree a = EmptyTree | Leaf a | Node a (Tree a) (Tree a)
```

Oto kilka przykładowych drzew:

```haskell
tree :: Tree String
tree = Node "a" (Node "b" EmptyTree (Leaf "c")) (Node "d" EmptyTree EmptyTree)

tree2 :: Tree Int
tree2 = Node 1 (Node 5 EmptyTree (Leaf 7)) (Node 3 EmptyTree EmptyTree)
```

Implementacja interfejsu `Foldable` dla drzewa:

```haskell
instance Foldable Tree where
    -- foldMap :: Monoid m => (a -> m) -> Tree a -> m
    -- Mapuje każdy element drzewa na monoid i łączy wyniki
    foldMap _ EmptyTree = M.mempty  -- puste drzewo to element neutralny monoidu
    foldMap f (Leaf x) = f x  -- liść to po prostu wartość przekształcona przez f
    foldMap f (Node x left right) = foldMap f left `M.mappend` f x `M.mappend` foldMap f right
    
    -- foldr :: (a -> b -> b) -> b -> Tree a -> b
    -- Składa drzewo "od prawej"
    foldr _ seed EmptyTree = seed  -- puste drzewo zwraca wartość początkową
    foldr f seed (Leaf x) = f x seed  -- liść aplikuje funkcję f do swojej wartości i seed
    foldr f seed (Node x left right) = foldr f (f x (foldr f seed right)) left
    -- Kolejność obliczania: najpierw prawe poddrzewo, potem węzeł, potem lewe poddrzewo
```

Implementując `Foldable` dla naszego drzewa, zyskujemy dostęp do wszystkich funkcji, 
które działają na typach implementujących ten interfejs, takich jak `fold`, `foldMap`, `foldr`, `foldl` itd.

### Przykłady Użycia Złożeń na Drzewach

Aby zademonstrować działanie `Foldable` na drzewach, zdefiniujmy typ pomocniczy `Any` do sprawdzania, czy drzewo zawiera określoną wartość:

```haskell
newtype Any = Any { getAny :: Bool }

instance Semigroup Any where 
 (<>) (Any x) (Any y) = Any (x || y)
 
instance Monoid Any where
  mempty = Any False 
```

Teraz możemy napisać funkcję sprawdzającą, czy drzewo zawiera określoną wartość:

```haskell
treeContains :: Eq a => a -> Tree a -> Bool
treeContains x = getAny . foldMap (\y -> Any (y == x))
```

Przykłady:
```
> treeContains 7 tree2
True
> treeContains 10 tree2
False
```

Możemy również zebrać wszystkie wartości z drzewa do listy w porządku inorder (lewe poddrzewo, węzeł, prawe poddrzewo):

```haskell
treeToList :: Tree a -> [a]
treeToList = foldMap (\x -> [x])
```

Przykłady:
```
> treeToList tree
["b","c","a","d"]
> treeToList tree2
[5,7,1,3]
```

