# Programowanie funkcyjne

## Tomasz Brengos

Wykład 4


## Kod wykładu 
Basics/Lecture04.hs


---

# Funktory Aplikatywne (Applicative)

Zanim przejdziemy do monad, warto zrozumieć functory aplikatywne, które są istotnym ogniwem pośrednim w hierarchii typów.

```haskell
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

Funktor aplikatywny to funktor rozszerzony o dwie operacje:
- `pure` - umieszcza wartość w kontekście (podobnie jak późniejszy `return`)
- `(<*>)` - aplikuje funkcję znajdującą się w kontekście do wartości w kontekście

## Aksjomaty dla Applicative:
```haskell
pure id <*> v = v                            -- tożsamość
pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- kompozycja
pure f <*> pure x = pure (f x)               -- homomorfizm
u <*> pure y = pure ($ y) <*> u              -- zamiana
```

---

# Przykłady Applicative

## Maybe jako Applicative:
```haskell
instance Applicative Maybe where
  pure = Just
  
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something
```

## Wykorzystanie:
```haskell
-- Połączenie dwóch wartości Maybe
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y

-- Przykład:
createUser :: Maybe String -> Maybe Int -> Maybe User
createUser name age = liftA2 User name age
```

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

