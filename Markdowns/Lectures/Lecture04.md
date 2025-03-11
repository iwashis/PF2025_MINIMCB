# Programowanie funkcyjne

## Tomasz Brengos

Wykład 4


## Kod wykładu 
Basics/Lecture04.hs

---

# Monady!

Przypomnienie definicji z teorii kategorii. Trójkę 
``` haskell 
(m, join, return)
```
nazywamy monadą, jeśli m jest funktorem oraz:
```haskell
join   :: m (m a)   -> m a
return ::  a        -> m a
```
spełniają znane aksjomaty dla mnożenia i jedności monady:
```haskell
join . (fmap join)   = join . join
join . (fmap return) = join . return = id
```

---

W Haskellu bardziej przydatne jest składanie w kategorii Kleisli
dla danej monady:
```haskell
(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> a -> m c
```
Wyraźmy >=> za pomocą join!
```haskell
f >=> g = join. fmap g . f 
```
Okazuje się, że częściej W Haskellu używana jest operacji bind:
```haskell
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
```
Definicja instancji Monad zawiera:
```haskell
(>>=)  :: (Monad m) => m a -> (a -> m b) -> m b
return :: (Monad m) =>   a -> m a
```

## Związek między >=> i >>= :
```haskell
f >=> g = \x -> ( f x >>= g )

x >>= g = (const x >=> g) ()
```

---

# Zacznijmy od (prawie) najprostrzej monady, czyli Maybe

Definicja instancji monady Maybe:
```haskell
instance Monad Maybe where
  Nothing >>= _ = Nothing
  Just x  >>= f = f x

  return = Just
```
Pobawmy się kodem. Funkcje head i tail w Haskellu są częściowe. 
Możemy je poprawić:
```haskell 
head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:xs) = Just x

tail' :: [a] -> Maybe [a]
tail' []     = Nothing
tail' (x:xs) = Just xs
```

## Zadanie: 
Używając head' i tail' napisać funkcję która zwraca 3ci element
z wejściowej listy:
```haskell
third :: [a] -> Maybe a
```

