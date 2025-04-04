# Wykład 4 - Funktory Aplikatywne i Monady

## Funktory Aplikatywne

Przypomnijmy, że funktor to abstrakcja, która pozwala na aplikowanie funkcji do wartości w kontekście. 
Funktory aplikatywne rozszerzają tę koncepcję, umożliwiając aplikowanie funkcji, które same są w kontekście.

Z perspektywy teorii kategorii, możemy zdefiniować funktor aplikatywny jako funktor `F: C -> C` wyposażony w operacje:

```haskell
pure :: a -> F a  -- to samo co eta w monadzie
(<*>) :: F (a -> b) -> F a -> F b
```

plus prawa aplikatywne, o których powiemy później.

Funktor aplikatywny można rozumieć jako rozszerzenie zwykłego funktora, 
które pozwala na aplikowanie funkcji wewnątrz kontekstu do wartości w kontekście. Na przykład: `Just (+3) <*> Just 5 = Just 8`

### Przykład: Maybe jako Funktor Aplikatywny

Zdefiniujmy własną wersję typu `Maybe` i zaimplementujmy dla niego instancję `Applicative`:

```haskell
data Maybe' a = Just' a | Nothing'
    deriving (Show, Functor)

instance Applicative Maybe' where
    pure = Just'
    Nothing' <*> _ = Nothing'
    _ <*> Nothing' = Nothing'
    (Just' f) <*> (Just' x) = Just' (f x)
```

Przykłady użycia `Maybe'` jako funktor aplikatywny:

```
Prelude> Just' (+3) <*> Just' 5
Just' 8
Prelude> Just' (*) <*> Just' 5 <*> Just' 3
Just' 15
Prelude> Nothing' <*> Just' 5
Nothing'
```

### Własna Lista jako Funktor Aplikatywny

Zdefiniujmy teraz własny typ listy i zaimplementujmy dla niego instancję `Applicative`:

```haskell
data MyList a = Nil | Con a (MyList a)
    deriving (Show, Functor)

-- concat' łączy dwie listy typu MyList
concat' :: MyList a -> MyList a -> MyList a
concat' Nil list2 = list2
concat' (Con x xs) list2 = Con x (concat' xs list2)

instance Applicative MyList where
    pure x = Con x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Con f fs) <*> args = fmap f args `concat'` (fs <*> args)
```

Przykład użycia `MyList` jako funktor aplikatywny:

```
Prelude> Con (+1) (Con (*2) Nil) <*> Con 3 (Con 4 Nil)
Con 4 (Con 5 (Con 6 (Con 8 Nil)))
```

W tym przykładzie funkcje `(+1)` i `(*2)` zostały zaaplikowane do każdego elementu listy `[3, 4]`, dając w wyniku listę `[3+1, 4+1, 3*2, 4*2]`, czyli `[4, 5, 6, 8]`.

Dla porównania, w standardowych listach moglibyśmy zdefiniować operator `<*>` w następujący sposób:

```haskell
star :: [a -> b] -> [a] -> [b]
star fs args = [f x | (f, x) <- zip fs args]
```

Ten operator działa inaczej - aplikuje każdą funkcję tylko do odpowiadającego jej argumentu:

```
Prelude> star [(+1), (*2)] [3, 4]
[4, 8]
```

### Prawa Funktorów i Funktorów Aplikatywnych

Przypomnijmy, jak wyglądają klasy `Functor` i `Applicative`:

```haskell
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

class (Functor' f) => Applicative' f where
    pure' :: a -> f a
    liftA2' :: (a -> b -> c) -> f a -> f b -> f c
```

Funktory muszą spełniać następujące prawa:
- Identity: `fmap id == id`
  - Mapowanie tożsamości nie zmienia struktury
  - Przykład: `fmap id [1,2,3] = [1,2,3]`
- Composition: `fmap (g . h) == (fmap g) . (fmap h)`
  - Kolejność mapowania nie ma znaczenia
  - Przykład: `fmap ((*2) . (+1)) [1,2,3] = fmap (*2) (fmap (+1) [1,2,3]) = [4,6,8]`

Funktory aplikatywne muszą spełniać następujące prawa:
- Identity: `pure id <*> v == v`
  - Aplikowanie funkcji tożsamości nie zmienia wartości
  - Przykład: `pure id <*> [1,2,3] = [1,2,3]`
- Homomorphism: `pure f <*> pure x == pure (f x)`
  - Aplikowanie funkcji do wartości w "czystym" kontekście
  - Przykład: `pure (+3) <*> pure 5 = pure 8`
- Interchange: `u <*> pure y == pure ($ y) <*> u`
  - Kolejność aplikowania funkcji do wartości
  - Przykład: `[(*2), (+3)] <*> pure 5 = pure ($ 5) <*> [(*2), (+3)] = [10, 8]`
- Composition: `pure (.) <*> u <*> v <*> w == u <*> (v <*> w)`
  - Kompozycja funkcji w kontekście aplikatywnym

### Praktyczne Zastosowania Funktorów Aplikatywnych

Funktory aplikatywne są niezwykle przydatne w praktyce. Jednym z typowych zastosowań jest walidacja danych lub obsługa błędów:

```haskell
data User = User String Int
    deriving (Show)

createUser :: String -> Int -> User
createUser = User

-- Tworzenie użytkownika z wartościami Maybe - użyteczne gdy dane mogą być niepełne
maybeCreateUser :: Maybe String -> Maybe Int -> Maybe User
maybeCreateUser = liftA2 createUser

-- Tworzenie wielu użytkowników z list imion i wieku
createMultipleUsers :: [String] -> [Int] -> [User]
createMultipleUsers = liftA2 createUser
```

Przykłady:

```
Prelude> maybeCreateUser (Just "Jan") (Just 30)
Just (User "Jan" 30)
Prelude> maybeCreateUser Nothing (Just 30)
Nothing
Prelude> createMultipleUsers ["Jan", "Anna"] [30, 25]
[User "Jan" 30, User "Anna" 25]
```

## Monady

Monady są kolejnym poziomem abstrakcji, który rozszerza funkcjonalność funktorów aplikatywnych. Z perspektywy teorii kategorii, monada to funktor `T: C -> C` wyposażony w strukturę:

```
eta : x -> T x   (nazywane return w Haskellu)
mu : T T x -> T x  (nazywane join w Haskellu)
```

plus prawa monadyczne.

W Haskellu definiujemy monadę za pomocą klasy typów:

```haskell
class (Functor' m, Applicative' m) => Monad' m where
    return' :: a -> m a  -- to samo co pure z instancji Applicative
    bind' :: m a -> (a -> m b) -> m b  -- zamiast bind piszemy >>= w notacji infixowej
    -- join' :: m (m a) -> m a
```

Monada umożliwia sekwencyjne wykonywanie operacji w kontekście. 
Operator `>>=` (bind) pozwala na "wyciągnięcie" wartości z kontekstu, wykonanie na niej operacji i "zapakowanie" wyniku z powrotem do kontekstu.

### Przykłady Operacji Monadycznych

Zdefiniujmy kilka pomocniczych funkcji do pracy z monadą `Maybe`:

```haskell
toList :: a -> [a]
toList = pure

-- Funkcja obliczająca pierwiastek, która może się nie powieść dla liczb ujemnych
fun :: Float -> Maybe Float
fun n
    | n >= 0 = Just $ sqrt n
    | otherwise = Nothing

-- Przykładowa funkcja kwadratowa zawsze się powiedzie
g :: Float -> Maybe Float
g n = Just $ n * n
```

Teraz zaimplementujmy instancję `Monad` dla naszego typu `Maybe'`:

```haskell
instance Monad Maybe' where
    -- >>= : Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing' >>= _ = Nothing'
    (Just' x) >>= f = f x
```

Przykłady użycia monady `Maybe'`:

```
Prelude> Just' 16 >>= fun
Just' 4.0
Prelude> Just' (-4) >>= fun
Nothing'
Prelude> Nothing' >>= fun
Nothing'
Prelude> Just' 3 >>= fun >>= g
Just' 9.0
```

### Kompozycja Funkcji Monadycznych

Monady umożliwiają elegancką kompozycję funkcji, które operują w kontekście. Operator `>=>` (Kleisli composition) pozwala na sekwencyjne łączenie takich funkcji:

```haskell
-- dla f :: a -> Maybe b , g :: b -> Maybe c
-- f >=> g :: a -> Maybe c
(f >=> g) x = case (f x) of
 Nothing -> Nothing
 Just y  -> g y
```

Zobaczmy praktyczne zastosowanie tego operatora na przykładzie funkcji operujących na listach:

```haskell
-- Bezpieczna wersja funkcji head, która zwraca Nothing dla pustej listy
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

-- Bezpieczna wersja funkcji tail, która zwraca Nothing dla pustej listy
tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (_ : xs) = Just xs

-- Funkcja wyciągająca trzeci element listy, używająca operatora >=>
third = tail' >=> tail' >=> head'
```

Przykłady:

```
Prelude> third [1,2,3,4]
Just 3
Prelude> third [1,2]
Nothing
```

Możemy zaimplementować tę samą funkcję używając operatora `>>=`:

```haskell
third_bind list = (tail' list >>= tail') >>= head'
```

### Składnia do

Haskell oferuje specjalną składnię `do`, która czyni kod monadyczny bardziej czytelnym, przypominającym styl imperatywny:

```haskell
third_do list = do 
  ys <- tail' list  -- wyciągnij ogon listy
  zs <- tail' ys    -- wyciągnij ogon ogona
  head' zs          -- weź głowę podwójnego ogona
```

Ta implementacja jest równoważna poprzednim, ale dla bardziej złożonych operacji monadycznych składnia `do` jest znacznie bardziej czytelna.
