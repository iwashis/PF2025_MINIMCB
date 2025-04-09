# Programowanie funkcyjne

## Tomasz Brengos

Wykład 7


## Kod wykładu 
Basics/Lecture07.hs

---

# Własności (niektórych) monad

## Twierdzenie z TK
Niech (m, join, return) będzie monadą. Wtedy istnieje naturalna struktura
monad na następujących funktorach:
1. m . Maybe
2. m . (Either a)
3. s -> m ( , s)
4. Monoid w => m ( , w)
5. ...

## Uwaga:
Nie jest zawsze prawdą to, że złożenie dowolnych dwóch monad na danej kategorii
jest monadą!

---

# Monad Transformers

```haskell
m   ~>  t m -- nowa monada.
```

Formalnie, w Haskellu transfomery monad zdefiniowane są przez zdefiniowanie:

1. Parametrycznego typu danych
```haskell
t :: ( * -> * ) -> * -> *
```

2. Instancji klasy:
```haskell
class MonadTrans t where
  lift :: Monad m => m a -> t m a
```

3. Dla każdej monady m zdefiniowanie instancji klasy Monad dla t m.

## Rozważmy przykład transformaty MaybeT
```haskell
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
```

## Praca domowa
Napisać definicję instancji transformaty StateT.

---

# Praca z przykładami

Przykład 1. 
```haskell
type Cost = State Int

type ExtraCost = StateT Double Cost
```

Przykład 2. 
```haskell
type MaybeIO = MaybeT IO
```
