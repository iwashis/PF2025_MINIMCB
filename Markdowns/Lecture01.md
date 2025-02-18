# Programowanie funkcyjne

## Tomasz Brengos

Wykład 1




## Kod wykładu 
Basics/Lecture01.hs


---


# Zaliczenie przedmiotu
* Projekt zespołowy, (40 pktów, przydzielane na +-dziesiątych zajęciach)
* Dwa testy na laboratoriach, (2 x 15 pktów)

---

# Przykład działającego kodu w Haskellu

```haskell

quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (x:xs) = quicksort le ++ [x] ++ quicksort gr
  where
    le = filter (< x) xs
    gr = filter (>=x) xs

```
Można i tak:

```haskell
quicksort []     = []
quicksort (x:xs) = quicksort le ++ [x] ++ quicksort gr
  where
    le = filter (< x) xs
    gr = filter (>=x) xs

```


---

# Programowanie w Haskellu, kilka skojarzeń: 

* Teoria kategorii 
* Deklaratywne
* Bez efektów ubocznych
* λ - calculus
* Leniwe obliczanie

---

# Programowanie w Haskellu, kilka skojarzeń: 

## Teoria kategorii

Składamy strzałki:
```haskell
(.) :: (a -> b) -> (b -> c) -> (a -> c)
g . f x = g (f x)
```

Monady są wszechobecne:
```haskell
class Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a
```


Uwaga notacyjna:
```haskell
a -> b -> c = a -> (b->c) ~izomorficzne~ (a,b) -> c
```

---

# Programowanie w Haskellu, kilka skojarzeń: 

## Deklaratywne

*How vs. What* 

---

## Paradygmaty programowania: Imperatywne vs. Deklaratywne

Przykład imperatywnego kodu (Python):
```python
sum = 0
for x in list:
    sum += x
print(sum)
```

Przykład deklaratywnego (funkcyjnego) kodu (Haskell):
```haskell
sum = foldr (+) 0 list 
```

Inny przykład innego deklaratywnego kodu (Sql):
```sql
SELECT * FROM employees WHERE age >= 20;
```

---

# Programowanie w Haskellu, kilka skojarzeń: 

## Bez efektów ubocznych

*Definicja*
Funkcja (wyrażenie) ma *efekty uboczne*, jeśli zmienia ona stan
pewnych zmiennych poza swoim środowiskiem.

*Pytanie*
Niech f i g będą funkcjami napisanymi w Pythonie.
Czy spełniona jest następująca równość:
```python
f(5)+g(5) = g(5)+f(5) 
```

*Przykłady*
* Globalne zmienne
* I/O 
* ...

---

# Programowanie w Haskellu, kilka skojarzeń:

## λ - calculus

*Definicja* (notacja Haskellowa) 
e ::= x ∈ Variables |  λx -> e | e e' 

*Definicja* (simply typed λ-calculus)
Gramatyka typów:
t ::= t -> t' | t' ∈ BaseTypes 
Gramatyka wyrażeń λ:
e ::= x |  λx:t -> e | e e' | c ∈ ConstantsOfBaseTypes 

---

# Programowanie w Haskellu, kilka skojarzeń: 

## Leniwe obliczanie


⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢿⣿⣿⠿⠿⠿⠻⠿⢿⡿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⡿⠟⠉⠈⠉⠉⠄⢠⠄⠄⢀⠄⠄⡬⠛⢿⢿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⡿⡿⠉⠄⠄⠄⠄⠄⠄⠅⠄⠅⠄⠐⠄⠄⠄⠁⠤⠄⠛⢿⢿⣿⣿⣿⣿
⣿⣿⣿⠍⠄⠄⠄⠄⠄⠄⠄⠄⣀⣀⠄⣀⣠⣀⠄⢈⣑⣢⣤⡄⠔⠫⢻⣿⣿⣿
⣿⡏⠂⠄⠄⢀⣠⣤⣤⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣮⣔⠂⡙⣿⣿
⡿⠄⠄⣠⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⣈⣿
⠇⠄⢠⣿⣿⣿⣿⣿⡿⠿⣿⣿⣿⣿⣿⣿⣿⣿⡟⠿⠿⢿⡿⣿⣿⣿⣿⣿⡧⣼
⠄⠄⠽⠿⠟⠋⠁⠙⠄⢠⣿⡿⢿⣿⣿⣿⣿⣿⣷⡠⢌⣧⠄⠈⠛⠉⠛⠐⡋⢹
⠄⠄⠄⠄⠄⠄⠄⢀⣠⣾⡿⠑⠚⠋⠛⠛⠻⢿⣿⣿⣶⣤⡄⢀⣀⣀⡀⠈⠄⢸
⣄⠄⠄⠄⢰⣾⠟⠋⠛⠛⠂⠄⠄⠄⠄⠒⠂⠛⡿⢟⠻⠃⠄⢼⣿⣿⣷⠤⠁⢸
⣿⡄⠄⢀⢝⢓⠄⠄⠄⠄⠄⠄⠄⠄⠠⠠⠶⢺⣿⣯⣵⣦⣴⣿⣿⣿⣿⡏⠄⢸
⣿⣿⡀⠄⠈⠄⠄⠄⠠⢾⣷⣄⢄⣀⡈⡀⠠⣾⣿⣿⣿⣿⣿⣿⣿⡿⠿⢏⣀⣾
⣿⣿⣷⣄⠄⠄⠄⢀⠈⠈⠙⠑⠗⠙⠙⠛⠄⠈⠹⠻⢿⡻⣿⠿⢿⣝⡑⢫⣾⣿
⣿⣿⣿⣿⣿⣆⡀⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠄⠑⠐⠚⣨⣤⣾⣿⣿

---

## Leniwe obliczanie


Nieskończona lista jedynek:
```haskell
ones = 1:ones
```

Nieskończona lista kolejnych liczb naturalnych: 
```haskell
nats = [1..]    
```

*Zagadka*
```haskell
f = 0:1:[ x+y | (x,y) <- zip f (tail f)]
```


---

# Leniwe obliczanie (zabawa)

*Wyrażenie*
```
((2+3)+(1+4))+(5+6)
```

Innermost evaluation:
```
((2+3)+(1+4))+(5+6) -> (5+(1+4))+(5+6) -> 
        (5+5)+(5+6) -> 10+(5+6)        -> 
        10+11       -> 21 
```

Lazy evaluation:
```
((2+3)+(1+4))+(5+6) -> ((2+3)+(1+4))+11  ->
      ((2+3)+5)+11  -> (5+5)+11          -> 
      10+11         -> 21
```


---

# Leniwe obliczanie (zabawa)

Rozważmy funkcję napisaną w C i w Haskellu:

```c
int f(int x, int y) {
  if (x>0)
    return x-1;
  else 
    return x+1;
}
```


```haskell
f :: Int -> Int -> Int
f x y = if x > 0 then x-1 else x+1
```


---
# Leniwe obliczanie (zabawa)

Ta sama funkcja: 

```haskell
f :: Int -> Int -> Int
f x y = if x > 0 then x-1 else x+1
```

dodajmy jeszcze:

```haskell
val = f 1 (product [1..])
```

---

# Leniwe obliczanie (zabawa)


```haskell
length1 :: [a] -> Int
length1 []     = 0
length1 (x:xs) = 1 + (length1 xs)
```

i policzmy

```haskell
let x = product [1..] in length1 [1,x]
```
* Jaki będzie wynik?

---

# Leniwe obliczanie (zabawa)


Zmieńmy trochę naszą definicję length:
```haskell
length2 :: [Int] -> Int
length2 []     = 0
length2 (x:xs) = if x > 0 then 1 + (length2 xs) else 1 + (length2 xs)
```

i policzmy znowu:

```haskell
let x = product [1..] in length2 [1,x]
```
* Jaki teraz będzie wynik?

---

# Ewaluacja

*Przykład* 
```haskell
length1 [1,x]    -> length1 1:[x] -> 1+(length1 [x]) ->
1+(length1 x:[]) ->1+(1+length1 []) -> 1+(1+0)-> 1+1 -> 2
```

*Definicja*
Wyrażenie jest postaci normalnej (NF, normal form), jeśli nie da się go zredukować.

*Przykłady*
```haskell
5
2:3:[]
(2,'t')
\x->x+2
```


---

# Ewaluacja

*Definicja* 
Wyrażenie jest w Weak Head Normal Form (WHNF), jeśli jest \-abstrakcją lub
wyrażeniem w ktorym najbardziej zewnętrzny konstruktor jest obliczony.

*Przykłady*
```haskell
(1+1,2)
\x->x+2
5:whatever
Just (sum [1..10])
```

*Nie-przykłady*
```haskell
map (\x-> x*x) [1,2]
(+) 1 2
```

*Pytanie*
Czy wyrażenie 
```haskell
f
```
jest w WHNF (NF?), jeśli 
```haskell
f x = x*x
```

---

# Ewaluacja

*Sztuczka*
```haskell
seq x y  -- oblicza x do WHNF (drugi argument może być dowolny, np. ()) i zwraca y. 
```

Można sprawdzić działanie 
```haskell
ghci> let x = 2+3 :: Int
ghci> :sprint x

ghci> seq x ()
()
ghci> :sprint x

ghci> let y = map id [x]
ghci> :sprint y
ghci> seq y ()
()
ghci> :sprint y

ghci> y 
ghci> :sprint y
```

---

# Ewaluacja

*Ćwiczenie*
Przedstawić ciąg redukcji:
```haskell
map negate [1,2,3]
```

Ściągawka:
```haskell 
map ::(a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = (f x) : map f xs

negate :: (Num a) => a -> a
negate x = -x
```

*Ćwiczenie 2*
Przedstawić ciąg redukcji:
```haskell
(take 6 . map (+1)) [1..10]
```

---

# Ewaluacja

*Twierdzenie*
Leniwa ewaluacja działa w co najwyżej tylu krokach co ewaluacja gorliwa.

*Problem*
Pamięć! 

*Ćwiczenie* 
```haskell 
sum' []     = 0
sum' (x:xs) = x + sum' xs


sum' [1..100000000]


sumUp [1..100000000]
```


---


# Literatura

 * [School of Haskell](https://www.schoolofhaskell.com)
 * [Learn you a Haskell for Greater Good!](http://learnyouahaskell.com)


