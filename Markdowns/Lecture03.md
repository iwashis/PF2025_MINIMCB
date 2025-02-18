# Programowanie funkcyjne

## Tomasz Brengos

Wykład 3


## Kod wykładu 
Basics/Lecture03.hs


---

# Typeclasses

W REPL:
```haskell
ghci> :t (==)

ghci> :t show

ghci> :t (<)

ghci> :t read

ghci> :t fmap
```

---

# Definiowanie własnych instancji

```haskell

data List a = Empty | Head a (List a)

instance Show a => Show List a where
  ...

instance Eq a => Eq (List a) where
  ...

instance Functor List where
  ...

```

---

# Definiowanie własnych typeclasses

## Przyklady typeclass predefiniowanych w Haskellu (wersja skrócona):

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b 

class Show a where
  show :: a -> String

class Semigroup a where
  (<>) :: a -> a -> a

class Semigroup a => Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
  mappend = (<>)
```

---


# Rozszerzmy nasze definicje instancji dla List

```haskell
instance Semigroup (List a) where
...

instance Monoid (List a) where
...
```

---


# Funktory

Ćwiczenia:
```haskell
data Maybe1 a = Just a | Nothing -- Maybe a := a + {*}

instance Functor Maybe1 where
  fmap = ...
```

```haskell
data Either1 a b = Left a | Right b

instance Functor (Either1 a) where
  fmap = ...
```

---

# Folds (klasycznie)

Niech:
```haskell
(*)  :: a -> b -> b
(#)  :: b -> a -> b
seed :: b
ai   :: a
```
Rozważmy następujące przyporządkowania:
```haskell
foldl (#) seed [a1..an] -> ((..(seed#a1)#a2#..)#an
foldr (*) seed [a1..an] -> a1*(a2*..(an*seed))..) 
```
Ich definicje są następujące:
```haskell
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f seed []     =  seed
foldl f seed (x:xs) =  foldl f (f seed x) xs
```
oraz:
```haskell 
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f seed []     = seed
foldr f seed (x:xs) = f x (foldr seed xs)
```
Obie funkcje działają leniwie! Jedną z nich można przepisać do 
wersji gorliwej:
```haskell
foldl' f seed []     = seed
foldl' f seed (x:xs) = let z = f seed x in seq z (foldl' f z xs)
```

---

# Folds (klasycznie)

## Ćwiczenia

1) Używając funkcji foldr zdefiniowac funkcję 
```haskell
init :: [a] -> [[a]] 
```
zwracającą wszystkie prefiksy argumentu, np. 
```haskell
init "tomek" = [[],"t","to","tom","tome","tomek"]
```
2) Używając foldl zdefiniować 
```haskell 
approxE :: Int -> Double
```
która dla argumentu n zwraca przybliżoną wartość liczby Eulera 
(używajac klasycznego wzoru na sumę odwrotności silnii kolejnych liczb nautralnych)

## Ćwiczenia ciekawsze

3) Zapisać foldl używajac funkcji foldr. 

---

# Ćwiczenie 3 (definicja foldl za pomocą foldr) 

## Komentarz pomocniczy:

```haskell
foldl (#) seed [a1..an] -> ((..(seed#a1)#a2#..)#an
foldr (*) seed [a1..an] -> a1*(a2*..(an*seed))..) 
```

Rozważmy:

```haskell
f1 = \v -> v # a1
f2 = \v -> v # a2
...
fn = \v -> v # an
```

Co się stanie jak policzymy:
```haskell
(foldr (flip (.)) id [f1..fn]) seed   -- flip :: (a -> b -> c) -> (b -> a -> c) 
```

```haskell 
(foldr (flip (.)) id [f1..fn]) seed ->   (...( id . fn )..). f2) . f1 seed 
== (fn . fn-1 . .. . f1)  seed -> (.. (seed # a1) # a2 .. )# an  
```
Dokończyć zadanie!

---

# Foldables bardziej ogólnie

```haskell
data Tree a = EmptyTree | Leaf a | Node a (Tree a) (Tree a)
```
Przykład:
```haskell
tree :: Tree String
tree = Node "a" (Node "b" Empty (Leaf "c")) (Node "d" Empty Empty)
```
Żeby łatwiej zrozumieć tree spójrzmy na: 
```
          a
        /   \
       b     d
        \
         c
```
Spróbujmy napisać wersje foldr dla Tree a zamiast dla [a].

```haskell
foldr :: (a -> b -> b) -> b -> Tree a -> b
foldr f seed EmptyTree           = seed
foldr f seed (Leaf x)            = f x seed
foldr f seed (Node x left right) = foldr f (f x (foldr f seed right)) left
```

Możemy też napisać coś nowego!
```haskell
foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
```
Okazuje się, że używając foldr możemy wyrazić foldMap i odwrotnie!
Oznacza to, że wystarczy zdefiniować jedno z nich. 

## Ćwiczenie

Zdefiniować foldMap za pomocą foldr i odwrotnie (dla list).

---

# Foldable typeclass

```haskell
class Foldable t where
  foldr   :: (a -> b -> b) -> b -> t a -> b
  foldMap :: (Monoid m) => (a -> m) -> t a -> m
-- i inne:  
  fold    :: (Monoid m) => t m -> m
  foldl   :: (b -> a -> b) -> b -> t a -> b
  ...
```
Nie trzeba definiować obu: foldr i foldMap. Wystarczy jedno z nich, gdyż
minimalna definicja instancji Foldable zawierać ma definicję
```haskell
foldr
-- lub
foldMap
```


