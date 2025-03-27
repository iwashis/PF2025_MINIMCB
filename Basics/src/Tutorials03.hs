{-# LANGUAGE NamedFieldPuns #-}

module Tutorials03 () where

-- # ADT i typeclassy
--
-- 1. **Słownik oparty na drzewie binarnym z balansowaniem**
--
--     Zdefiniuj algebryczny typ danych reprezentujący drzewo wyszukiwań binarnych (BST),
--     które będzie służyło jako słownik mapujący klucze na wartości. Następnie zaimplementuj
--     następujące operacje:
--     - *Wstawianie*: Dodanie pary klucz-wartość.
--     - *Wyszukiwanie*: Pobranie wartości przypisanej do klucza.
--     - *Usuwanie*: Usunięcie klucza (oraz odpowiadającej mu wartości) z drzewa.
--     - *Aktualizacja*: Modyfikacja wartości przypisanej do klucza.
--     - *Balansowanie*: Zaimplementuj procedurę balansowania (np. wykorzystując algorytm drzewa
--         AVL lub czerwono-czarnego), aby drzewo pozostało zbalansowane po operacjach wstawiania i usuwania.
data BST k v
    = Empty
    | BST
        { key :: k
        , value :: v
        , left :: BST k v
        , right :: BST k v
        }
    deriving (Show)

insert :: (Ord k) => BST k v -> (k, v) -> BST k v
insert Empty (key, value) = BST{key, value, left = Empty, right = Empty}
insert BST{key, value, left, right} (k, v)
    | k < key = BST{key, value, left = insert left (k, v), right}
    | otherwise = BST{key, value, left, right = insert right (k, v)}

fromList :: (Ord k) => [(k, v)] -> BST k v
fromList = foldl insert Empty

example :: [(Int, Int)]
example = [(1, 5), (4, 4), (3, 16), (2, 2)]

find :: (Ord k) => BST k v -> k -> Maybe v
find Empty _ = Nothing
find BST{key, value, left, right} k
    | k < key = find left k
    | k > key = find right k
    | otherwise = Just value

-- w tym przypadku zakladamy, ze klucze w drzewie sa unikatowe
update :: (Ord k) => BST k v -> (k, v) -> BST k v
update Empty _ = Empty
update BST{key, value, left, right} (k, v)
    | k < key = BST{key, value, left = update left (k, v), right}
    | k > key = BST{key, value, left, right = update right (k, v)}
    | otherwise = BST{key, value = v, left, right}

-- TODO: usuwanie i balansowanie

-- 2. **Interpreter wyrażeń z różniczkowaniem i upraszczaniem**
--
--     Zdefiniuj algebraiczny typ danych reprezentujący wyrażenia arytmetyczne
--     (uwzględniający zmienne, stałe, dodawanie, mnożenie i potęgowanie). Napisz funkcje, które:
--     - *Ewaluacja*: Obliczają wartość numeryczną wyrażenia, korzystając z mapowania zmiennych na liczby.
--     - *Różniczkowanie*: Symbolicznie różniczkują wyrażenie względem danej zmiennej.
--     - *Upraszczanie*: Redukują wyrażenie do prostszej formy poprzez stosowanie uproszczeń algebraicznych
--     (np. eliminowanie składników zerowych, łączenie wyrazów podobnych).

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) | Mul (Expr a) (Expr a)

exampleExpr :: Expr String
exampleExpr = Mul (Add (Var "x") (Val 5)) (Val 7)
-- (x + 5)*7

eval :: (a -> Int) -> Expr a -> Int
eval context (Var x) = context x
eval _ (Val i) = i
eval context (Add l r) = eval context l + eval context r
eval context (Mul l r) = eval context l * eval context r

derivative :: Expr a -> a -> Expr a
derivative = undefined -- TODO

-- 3. **Własna leniwa lista z obsługą nieskończoności**
--
--     Stwórz własny typ listy (np. `data MyList a = Nil | Cons a (MyList a)`), który wspiera leniwą ewaluację.
--     Zaimplementuj następujące funkcje:
--     - `myMap`: Funkcję analogiczną do `map`.
--     - `myFoldr`: Funkcję złożenia prawego (`foldr`), która potrafi działać na nieskończonych listach, jeśli to możliwe.
--     - `myFilter`: Funkcję analogiczną do `filter`.
--     Następnie zdefiniuj instancje `Functor, Foldable` dla `Mylist`.

data MyList a = Nil | Cons a (MyList a)

exampleList :: MyList Int
exampleList = Cons 5 (Cons 6 Nil)

myMap :: (a -> b) -> MyList a -> MyList b
myMap _ Nil = Nil
myMap f (Cons x xs) = Cons (f x) (myMap f xs)

instance Functor MyList where
    fmap = myMap

-- foldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr :: (a -> b -> b) -> b -> MyList a -> b
myFoldr _ seed Nil = seed
myFoldr f seed (Cons x xs) = f x (myFoldr f seed xs)

instance Foldable MyList where
    foldr = myFoldr

-- TODO:
--  `myFilter`: Funkcję analogiczną do `filter`.
--


-- 5. **Znane typeclassy dla drzew**
--
--     Zdefiniuj instancje `Show, Eq, Semigroup, Monoid, Foldable, Functor` dla parametrycznego typu danych
--     `data T a = EmptyT | LeafT a | InnerT (T a) (T a)`.

data T a = EmptyT | LeafT a | InnerT (T a) (T a)

-- deriving (Show,Eq)

instance (Show a) => Show (T a) where
    show tree = "{" ++ go tree ++ "}"
      where
        go EmptyT = ""
        go (LeafT x) = show x
        go (InnerT left right) =
            let
                l = go left
                r = go right
             in
                l ++ (if r /= "" then ", " ++ r else "")

exampleTree3 :: T Int
exampleTree3 = InnerT (LeafT 10) (InnerT (LeafT 13) (LeafT 14))

exampleTree4 :: T Int
exampleTree4 = InnerT (LeafT 1) (InnerT (LeafT 13) (LeafT 14))

instance (Eq a) => Eq (T a) where
    EmptyT == EmptyT = True
    EmptyT == _ = False
    _ == EmptyT = False
    (LeafT x) == (LeafT y) = x == y
    (LeafT _) == (InnerT _ _) = False
    (InnerT _ _) == (LeafT _) = False
    (InnerT xl xr) == (InnerT yl yr) = (xl == yl) && (xr == yr)

-- to jest oszukany przyklad, bo nie mamy łączności
instance Semigroup (T a) where
    t <> s = InnerT t s

-- TODO: jak zmienic typ danych T, tak zeby mialo strukture drzewiasta, ale tez zeby taka operacja jak wyzej
-- spelniala prawo lacznosci

-- tak samo oszukany
instance Monoid (T a) where
    mempty = EmptyT

-- TODO: jak zmienic typ danych T, tak zeby mialo strukture drzewiasta, ale tez zeby
-- dalo sie na niech zdefiniowac pelna strukture monoidu

instance Functor T where
    -- fmap :: (a-> b) -> T a -> T b
    fmap _ EmptyT = EmptyT
    fmap f (LeafT x) = LeafT (f x)
    fmap f (InnerT l r) = InnerT (fmap f l) (fmap f r)

instance Foldable T where
    -- foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
    foldMap _ EmptyT = mempty
    foldMap f (LeafT x) = f x
    foldMap f (InnerT l r) = foldMap f l `mappend` foldMap f r

-- TODO: foldr
