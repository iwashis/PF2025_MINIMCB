# Algebraiczne Typy Danych

## Typy Wyliczeniowe

Haskell posiada wbudowane typy danych, takie jak `Bool` zdefiniowany jako:

```haskell
-- data Bool = True | False
```

Na podobnej zasadzie możemy definiować własne typy wyliczeniowe. Poniżej definiujemy typ `BasicColors` reprezentujący podstawowe kolory:

```haskell
data BasicColors = Red | Blue | Green
```

`BasicColors` ma trzy wartości konstruktora: `Red`, `Blue` i `Green`. Możemy używać go tak:

```
> let kolor = Red
> :t kolor
kolor :: BasicColors
```

## Algebraiczne Typy Danych

Algebraiczne typy danych (ADT) pozwalają na definiowanie bardziej złożonych struktur. Przyjrzyjmy się typowi `Shape` reprezentującemu różne kształty:

```haskell
data Shape
    = Rectangle (Double, Double) (Double, Double)
    | Circle (Double, Double) Double
    | Point (Double, Double)
```

Typ `Shape` ma trzy różne konstruktory:
- `Rectangle` przyjmuje dwie pary współrzędnych (lewy dolny róg i prawy górny róg)
- `Circle` przyjmuje parę współrzędnych środka i promień
- `Point` przyjmuje parę współrzędnych punktu

Przykłady użycia:

```
> let kwadrat = Rectangle (0,0) (5,5)
> let kolo = Circle (3,3) 2
> let punkt = Point (1,1)
```

Teraz możemy zdefiniować funkcje operujące na tych kształtach. Na przykład, funkcja obliczająca pole powierzchni:

```haskell
volume :: Shape -> Double
volume (Point (_, _)) = 0
volume (Circle _ r) = pi * (r ^ 2)
volume (Rectangle (x, y) (z, t)) = abs (z - x) * abs (t - y)
```

Przykłady:

```
> volume (Point (1,1))
0
> volume (Rectangle (0,0) (3,4))
12.0
> volume (Circle (0,0) 2)
12.566370614359172
```

Możemy również definiować funkcje transformujące kształty, jak ta zwiększająca promień koła o 5 (jeśli kształt jest kołem):

```haskell
changeRadiusIfCircle :: Shape -> Shape
changeRadiusIfCircle (Circle point r) = Circle point (r + 5)
changeRadiusIfCircle x = x
```

Przykłady:

```
> changeRadiusIfCircle (Circle (0,0) 2)
Circle (0,0) 7
> changeRadiusIfCircle (Rectangle (0,0) (3,4))
Rectangle (0,0) (3,4)
```

## Rekordy

Haskell oferuje wygodny sposób definiowania typów danych z nazwanymi polami za pomocą rekordów. Porównajmy dwa sposoby definiowania typu `Person`:

Najpierw standardowa definicja typu:

```haskell
data Person = Person String String Int
    deriving (Show)

p = Person "Tomek" "Kowalski" 25
```

A teraz definicja z użyciem rekordów:

```haskell
data Person2 = Person2
    { name :: String
    , surname :: String
    , age :: Int
    }
    deriving (Show)

p2 = Person2 "Tomek" "Kowalski" 25
```

Rekordy automatycznie tworzą funkcje dostępu do pól, co ułatwia pracę z danymi:

```
> name p2
"Tomek"
> surname p2
"Kowalski"
> age p2
25
```

Możemy również używać składni RecordWildCards do wygodniejszej pracy z rekordami:

```haskell
addMr :: Person2 -> Person2
addMr p@Person2{name = name, surname = surname, ..} =
    if surname /= ""
        then Person2{name = "Mr." ++ name, ..}
        else p
```

Funkcja ta dodaje przedrostek "Mr." do imienia osoby, o ile nazwisko nie jest puste. Użycie `..` pozwala zachować pozostałe pola bez zmian.

Przykłady:

```
> addMr (Person2 "Jan" "Kowalski" 30)
Person2 {name = "Mr.Jan", surname = "Kowalski", age = 30}
> addMr (Person2 "Jan" "" 30)
Person2 {name = "Jan", surname = "", age = 30}
```

## Rekurencyjne Struktury Danych

W Haskellu możemy definiować rekurencyjne struktury danych. 
Zobaczmy, jak zdefiniować własną listę liczb całkowitych:

```haskell
data IntList = EmptyIntList | NonEmptyList Int IntList
    deriving (Show)

listExample = NonEmptyList 4 (NonEmptyList 5 EmptyIntList)
```

Lista składa się z pustej listy (`EmptyIntList`) lub niepustej listy (`NonEmptyList`), 
która zawiera wartość typu `Int` i resztę listy. 
Możemy teraz definiować funkcje operujące na tej strukturze, jak obliczanie długości listy:

```haskell
length2 :: IntList -> Int
length2 EmptyIntList = 0
length2 (NonEmptyList _ list) = 1 + length2 list
```

Przykłady:

```
> length2 EmptyIntList
0
> length2 (NonEmptyList 1 (NonEmptyList 2 EmptyIntList))
2
> length2 listExample
2
```

Podobnie możemy zdefiniować drzewo binarne dla liczb całkowitych:

```haskell
data IntTree = EmptyTree | IntNode Int IntTree IntTree
```

## Typy Parametryzowane

Definiowanie oddzielnych typów dla list czy drzew różnych typów byłoby nieefektywne. 
Haskell umożliwia definiowanie typów parametryzowanych, które działają z dowolnym typem. Zdefiniujmy ogólne drzewo binarne:

```haskell
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

exampleTree :: Tree [Int]
exampleTree = Node [1, 2] Empty (Node [4] Empty Empty)
```

Typ `Tree a` oznacza drzewo przechowujące wartości typu `a`. Możemy tworzyć drzewa przechowujące liczby, napisy, listy, a nawet inne drzewa!

```
> Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty) :: Tree Int
Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)
> Node "root" (Node "left" Empty Empty) (Node "right" Empty Empty) :: Tree String
Node "root" (Node "left" Empty Empty) (Node "right" Empty Empty)
```

Podobnie zdefiniujmy ogólną listę dla dowolnego typu:

```haskell
data List a = EmptyList | Head a (List a)
```

## Własne Implementacje Klas Typów

Jedną z najbardziej potężnych cech Haskella jest możliwość definiowania własnych implementacji klas typów. Przyjrzyjmy się kilku przykładom:

### Show - Konwersja na String

Możemy zdefiniować własną implementację `Show` dla naszego typu `List`:

```haskell
instance (Show a) => Show (List a) where
    show EmptyList = ""
    show (Head a list) = show a ++ "," ++ show list
```

Teraz możemy wyświetlać nasze listy w przyjazny (ale też dość pokraczny) sposób:

```
> Head 1 (Head 50 EmptyList)
1,50,
```

### Eq - Porównywanie Wartości

Możemy również zdefiniować porównywanie dla naszych list:

```haskell
instance (Eq a) => Eq (List a) where
    EmptyList == EmptyList = True
    (Head a _) == EmptyList = False
    EmptyList == (Head a _) = False
    (Head a list1) == (Head b list2) = a == b && list1 == list2
```

Teraz możemy porównywać nasze listy:

```
> Head 1 (Head 2 EmptyList) == Head 1 (Head 2 EmptyList)
True
> Head 1 (Head 2 EmptyList) == Head 1 (Head 3 EmptyList)
False
```

## Aliasy Typów

Haskell pozwala tworzyć aliasy typów, które poprawiają czytelność kodu:

```haskell
type NewInt = Int

type Width = Int
type Height = Int

volume2 :: Width -> Height -> Int
volume2 h w = h * w
```

Aliasy typów nie tworzą nowych typów, a jedynie nowe nazwy dla istniejących typów. Są one szczególnie przydatne do poprawy czytelności sygnatur funkcji:

```
> volume2 3 4
12
```

## Funktory, Półgrupy i Monoidy

Na koniec przyjrzyjmy się bardziej zaawansowanym abstrakcjom, które stanowią fundament programowania funkcyjnego.

### Funktor

Funktor jest abstrakcją, która pozwala na aplikowanie funkcji do wartości opakowanych w kontekst:

```haskell
instance Functor List where
    -- fmap :: ( a -> b ) -> ( List a -> List b)
    fmap _ EmptyList = EmptyList
    fmap f (Head a list) = Head (f a) (fmap f list)
```

Teraz możemy używać funkcji `fmap` do mapowania funkcji na wszystkie elementy naszej listy:

```
> fmap (+1) (Head 1 (Head 2 EmptyList))
2,3,
> fmap show (Head 1 (Head 2 EmptyList))
"1","2",
```

### Półgrupy i Monoidy

Półgrupa to struktura algebraiczna z operacją łączenia, a monoid to półgrupa z elementem neutralnym:

```haskell
instance Semigroup (List a) where
    -- (<>) :: List a -> List a -> List a
    EmptyList <> ys = ys
    (Head x xs) <> ys = Head x (xs <> ys)

instance Monoid (List a) where
    -- mempty :: List a
    mempty = EmptyList
    -- mappend :: List a -> List a -> List a
    mappend = (<>)
```

Teraz możemy łączyć nasze listy za pomocą operatora `<>`:

```
> Head 1 (Head 2 EmptyList) <> Head 3 (Head 4 EmptyList)
1,2,3,4,
> mempty <> Head 1 (Head 2 EmptyList)
1,2,
> Head 1 (Head 2 EmptyList) <> mempty
1,2,
```

