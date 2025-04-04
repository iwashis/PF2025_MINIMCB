# Wykład 1 - Wprowadzenie do Programowania Funkcyjnego w Haskellu


## Interaktywna Praca z GHC

Haskell posiada interaktywny interpreter GHCi (Glasgow Haskell Compiler Interactive), który pozwala na eksperymentowanie z kodem i natychmiastowe uzyskiwanie wyników. Aby uruchomić GHCi, wystarczy wpisać `ghci` w terminalu:

```
$ ghci
GHCi, version 8.10.7: https://www.haskell.org/ghc/  :? for help
Prelude>
```

W GHCi możemy wprowadzać wyrażenia i natychmiast otrzymywać ich wartości:

```
Prelude> 2 + 2
4
Prelude> "Hello, " ++ "world!"
"Hello, world!"
```

## Typy i System Typów

Jedną z najważniejszych cech Haskella jest jego silny statyczny system typów. Każde wyrażenie w Haskellu ma określony typ, który jest sprawdzany podczas kompilacji. Możemy sprawdzić typ wyrażenia za pomocą komendy `:t` lub `:type`:

```
Prelude> :t 42
42 :: Num p => p
Prelude> :t "Haskell"
"Haskell" :: [Char]
Prelude> :t True
True :: Bool
```

Zwróćmy uwagę, że typ liczby `42` jest polimorficzny - może być dowolnym typem numerycznym (`Int`, `Integer`, `Float`, itd.). Natomiast typ napisu "Haskell" to `[Char]`, czyli lista znaków.

Możemy również definiować własne zmienne i sprawdzać ich typy:

```
Prelude> let x = 10
Prelude> let s = "Programowanie funkcyjne"
Prelude> :t x
x :: Num p => p
Prelude> :t s
s :: [Char]
```

W Haskellu możemy także jawnie podawać typy zmiennych:

```
Prelude> let y :: Int; y = 20
Prelude> :t y
y :: Int
```

## Quicksort

Przyjrzyjmy się eleganckiej implementacji algorytmu quicksort w Haskellu. Algorytm ten jest doskonałym przykładem zwięzłości i deklaratywności programowania funkcyjnego. Działa on rekurencyjnie według następującej zasady:

1. Jeśli lista jest pusta, zwracamy pustą listę (przypadek bazowy)
2. W przeciwnym razie wybieramy pierwszy element jako pivot
3. Dzielimy pozostałe elementy na dwie grupy: mniejsze od pivota i większe/równe od pivota
4. Sortujemy rekurencyjnie obie podlisty i łączymy je z pivotem w środku

Cała implementacja zajmuje zaledwie kilka linijek kodu:

```haskell
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) = quicksort le ++ [x] ++ quicksort gr
  where
    le = filter (< x) xs
    gr = filter (>= x) xs
```

Warto zwrócić uwagę na sygnaturę typu `(Ord a) => [a] -> [a]`, która oznacza, że funkcja przyjmuje listę elementów dowolnego typu `a`, pod warunkiem że typ ten należy do klasy typów `Ord` (czyli jego elementy można porównywać), i zwraca listę elementów tego samego typu.

Spróbujmy teraz wykonać kilka przykładów w GHCi:

```
Prelude> quicksort [3,1,4,1,5,9,2,6]
[1,1,2,3,4,5,6,9]
Prelude> quicksort "haskell"
"aehklls"
Prelude> quicksort []
[]
Prelude> quicksort [7,7,7,7]
[7,7,7,7]
Prelude> quicksort [-10,5,0,-3,8]
[-10,-3,0,5,8]
```

## Currying Funkcji

W językach funkcyjnych, takich jak Haskell, funkcje są "curried" (od nazwiska matematyka Haskella Curry'ego). Oznacza to, że funkcja przyjmująca wiele argumentów jest traktowana jako seria funkcji jednoargumentowych.

Rozważmy funkcję dodającą dwie liczby:

```haskell
add :: Int -> (Int -> Int)
add x = \y -> x + y
```

Sygnatura typu `Int -> (Int -> Int)` mówi nam, że `add` przyjmuje liczbę całkowitą i zwraca funkcję, która z kolei przyjmuje liczbę całkowitą i zwraca liczbę całkowitą. Innymi słowy, `add` przyjmuje argument `x` i zwraca funkcję, która przyjmuje argument `y` i zwraca sumę `x + y`.

W praktyce możemy używać tej funkcji na dwa sposoby:

```
Prelude> add 2 3
5
Prelude> (add 2) 3
5
```

W pierwszym przypadku od razu podajemy oba argumenty. W drugim najpierw tworzymy funkcję częściowo zaaplikowaną `(add 2)`, a następnie aplikujemy ją do argumentu `3`.

Warto zauważyć, że w Haskellu wszystkie funkcje są domyślnie curried, więc można również zapisać tę funkcję krócej:

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

## Częściowa Aplikacja Funkcji

Currying pozwala na częściową aplikację funkcji, czyli utworzenie nowej funkcji przez dostarczenie tylko części argumentów do funkcji wieloargumentowej.

Na przykład, możemy utworzyć funkcję, która dodaje 6 do swojego argumentu:

```haskell
t = add 6
```

Teraz `t` jest funkcją jednoargumentową:

```
Prelude> t 4
10
Prelude> t 0
6
Prelude> map t [1,2,3]
[7,8,9]
```

Częściowa aplikacja jest niezwykle przydatna w programowaniu funkcyjnym, ponieważ pozwala na tworzenie nowych funkcji w prosty sposób i ułatwia komponowanie funkcji.

Możemy również częściowo aplikować funkcje wbudowane:

```
Prelude> let addOne = (+ 1)
Prelude> addOne 10
11
Prelude> let isPositive = (> 0)
Prelude> isPositive 5
True
Prelude> isPositive (-3)
False
```

## Nieskończone Listy

Jedną z unikalnych cech Haskella jest leniwa ewaluacja, która pozwala na pracę z nieskończonymi strukturami danych. Dzięki temu możemy definiować nieskończone listy, które są obliczane tylko w takim stopniu, w jakim jest to potrzebne.

Oto przykład nieskończonej listy jedynek:

```haskell
ones = 1 : ones
```

Ta definicja może wydawać się rekurencyjna i nieskończona, i dokładnie tak jest! Dzięki leniwej ewaluacji Haskell oblicza tylko tyle elementów, ile jest potrzebne:

```
Prelude> take 5 ones
[1,1,1,1,1]
Prelude> sum (take 100 ones)
100
```

Innym przykładem jest nieskończona lista liczb naturalnych:

```haskell
naturals = 0 : map (+1) naturals
```

Możemy teraz łatwo pobrać dowolną liczbę liczb naturalnych:

```
Prelude> take 10 naturals
[0,1,2,3,4,5,6,7,8,9]
```

Lub znaleźć liczby parzyste:

```
Prelude> take 10 (filter even naturals)
[0,2,4,6,8,10,12,14,16,18]
```

## Ciąg Fibonacciego

Ciąg Fibonacciego jest klasycznym przykładem rekurencji. W Haskellu możemy zdefiniować go w niezwykle elegancki sposób, wykorzystując listy składające (list comprehension) i rekurencję:

```haskell
fib = 0 : 1 : [x + y | (x, y) <- zip fib (drop 1 fib)]
```

Ta definicja jest niemal dokładnym odzwierciedleniem matematycznej definicji ciągu Fibonacciego: każdy element jest sumą dwóch poprzednich. Zauważmy, że definiujemy `fib` w kategoriach samego siebie!

Spróbujmy kilku przykładów:

```
Prelude> take 10 fib
[0,1,1,2,3,5,8,13,21,34]
Prelude> fib !! 6  -- element o indeksie 6 (siódmy element)
8
Prelude> fib !! 20
6765
```

Możemy również znaleźć elementy ciągu Fibonacciego, które spełniają określone warunki:

```
Prelude> take 5 (filter (>100) fib)
[144,233,377,610,987]
```

## Rekurencja 

Rekurencja jest podstawowym mechanizmem w programowaniu funkcyjnym, zastępującym tradycyjne pętle znane z języków imperatywnych. Przyjrzyjmy się prostej funkcji sumującej elementy listy:

```haskell
sum' :: [Int] -> Int
sum' [] = 0
sum' (x : xs) = x + sum' xs
```

Przetestujmy funkcję na kilku przykładach:

```
Prelude> sum' []
0
Prelude> sum' [1,2,3,4,5]
15
Prelude> sum' [-3,5,10]
12
```

W praktyce często używamy funkcji wyższego rzędu, takich jak `map`, `filter` czy `foldr`:

```
Prelude> map (*2) [1,2,3,4,5]  -- podwojenie każdego elementu
[2,4,6,8,10]
Prelude> filter even [1,2,3,4,5]  -- wybranie elementów parzystych
[2,4]
Prelude> foldr (+) 0 [1,2,3,4,5]  -- sumowanie (równoważne sum')
15
```
