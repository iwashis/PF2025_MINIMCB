# Programowanie funkcyjne

## Tomasz Brengos

Wykład 5


## Kod wykładu 
Basics/Lecture05.hs

---

# Monady cz. 2 

Przypomnienie (jeszcze raz) definicji z teorii kategorii. Trójkę 
``` haskell 
(m, join, return)
```
nazywamy monadą, jeśli m jest funktorem oraz:
```haskell
join   :: m (m a)   -> m a
return ::  a        -> m a
```
spełniają aksjomaty dla mnożenia i jedności monady:
```haskell
join . (fmap join)   = join . join
join . (fmap return) = join . return = id
```

## Jak zdefiniować >=> i >>= za pomocą join, fmap? 
```haskell 
f >=> g  = join . (fmap g) . f

xs >>= g = (join . fmap g) xs
```

---

# Notacja do

```haskell
xs >>= \x -> f x     


do 
  x <- xs
  f x
```

## Przyład 
```haskell
tail' xs >>= head'

do 
  ys <- tail' xs
  head' ys
```


---

# Monada []
Operacje return i >>= są w tym przypadku następujących typów:
```haskell
return : a -> [a]

>>=    : [a] -> (a -> [b]) -> [b]
```

## Ćwiczenie: napisać definicję instancji monady [-].

## Przykład
```haskell
triple = \x -> [x,x,x]
["Bunny"] >>= triple >>= triple
```

---

# Przykład zapisu w notacji do dla monady [-]
```haskell
fib = 0 : 1 : do
      (x,y) <- zip fib $ tail fib
      return x+y
```
W wersji syntactic sugar mamy:
```haskell
fib = 0 : 1 : [ x+y | (x,y) <- zip fib $ tail fib ]
```

---

# State monad: State s a = a -> (a,s)

```haskell
State s a = State { runState :: s -> (a,s) }
```

---

# Ćwiczenie: Napisać instancje Functor i Monad dla State s a

1. Definiujemy nowy typ danych State1 s a izomorficzny z wyjściowym.
2. (Dygresja!) Interpretujemy komunikat HLS dotyczący data vs. newtype.

Przykład:
```haskell
data Point    = Point (Double, Double)

newtype Point = Point (Double, Double)
```
Różnica reprezentacji w runtime:
```haskell
   Point                     (,)
     |                      /   \ 
    (,)                Double   Double
   /   \                                    
 Double Double             
```

3. Definiujemy instancje Functor i Monad dla State s a.



