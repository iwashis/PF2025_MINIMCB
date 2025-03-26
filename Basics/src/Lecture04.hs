{-# LANGUAGE DeriveFunctor #-}
module Lecture04 where
import Control.Monad ((>=>)) 
-- monada - funktor T: C -> C wyposażony w strukturę
-- eta : x -> T x
-- mu : T T x -> T x
-- + prawa monadyczne
--

-- aplicative - funktor F : C -> C
-- nie chcemy definiować tego formalnie w języku teorii kategorii i zdefiniujemy tylko
-- haskellowo
--
-- pure : x -> F x  -- to samo co eta w monadzie
-- (<*>) :: f (a -> b) -> f a -> f b
--  + prawa aplikatywne
-- Tw. jeśli funktor F ma strukturę monadyczną, to ona indukuje naturalną strukturę applicative.
-- 
-- Funktor aplikatywny można rozumieć jako rozszerzenie zwykłego funktora,
-- które pozwala na aplikowanie funkcji wewnątrz kontekstu do wartości w kontekście.
-- Na przykład: Just (+3) <*> Just 5 = Just 8

-- Przykład 1
--
data Maybe' a = Just' a | Nothing'
    deriving (Show, Functor)
instance Applicative Maybe' where
    pure = Just'
    Nothing' <*> _ = Nothing'
    _ <*> Nothing' = Nothing'
    (Just' f) <*> (Just' x) = Just' (f x)

-- Przykłady użycia Maybe' jako Applicative:
-- Just' (+3) <*> Just' 5 = Just' 8
-- Just' (*) <*> Just' 5 <*> Just' 3 = Just' 15
-- Nothing' <*> Just' 5 = Nothing'

-- typ izomorficzny z [a]
data MyList a = Nil | Con a (MyList a)
    deriving (Show, Functor)

-- concat' łączy dwie listy typu MyList
-- Przykład: concat' (Con 1 (Con 2 Nil)) (Con 3 Nil) = Con 1 (Con 2 (Con 3 Nil))
concat' :: MyList a -> MyList a -> MyList a
concat' Nil list2 = list2
concat' (Con x xs) list2 = Con x (concat' xs list2)

instance Applicative MyList where
    pure x = Con x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Con f fs) <*> args = fmap f args `concat'` (fs <*> args)

-- Przykład użycia MyList jako Applicative:
-- Con (+1) (Con (*2) Nil) <*> Con 3 (Con 4 Nil) = Con 4 (Con 5 (Con 6 (Con 8 Nil)))

-- moglibyśmy zdefiniować <*> w notacji listowej jako:
-- fs <*> args = [f x | (f,x) <- zip fs args]
star :: [a -> b] -> [a] -> [b]
star fs args = [f x | (f, x) <- zip fs args]

-- Przykład użycia star:
-- star [(+1), (*2)] [3, 4] = [4, 8]
-- To pokazuje, jak każda funkcja zostaje zastosowana do odpowiadającego jej argumentu.

-- Funktory, Applicative: podsumowanie
--
-- Functor:
class Functor' f where
    fmap' :: (a -> b) -> f a -> f b
-- dla f : a -> b, mamy zadaną F(f): Fa -> Fb
-- F(id) = id
-- F( f . g ) = F(f) . F(g)
--
-- Prawa funktora można interpretować następująco:
-- Identity: fmap id == id
--   - Mapowanie tożsamości nie zmienia struktury
--   - Przykład: fmap id [1,2,3] = [1,2,3]
-- Composition: fmap (g . h) == (fmap g) . (fmap h)
--   - Kolejność mapowania nie ma znaczenia
--   - Przykład: fmap ((*2) . (+1)) [1,2,3] = fmap (*2) (fmap (+1) [1,2,3]) = [4,6,8]

class (Functor' f) => Applicative' f where
    pure' :: a -> f a
    -- (<*>) :: f (a -> b) -> f a -> f b
    liftA2' :: (a -> b -> c) -> f a -> f b -> f c

-- Prawa aplikatywne można rozumieć jako:
-- Identity: pure id <*> v == v
--   - Aplikowanie funkcji tożsamości nie zmienia wartości
--   - Przykład: pure id <*> [1,2,3] = [1,2,3]
-- Homomorphism: pure f <*> pure x == pure (f x)
--   - Aplikowanie funkcji do wartości w "czystym" kontekście
--   - Przykład: pure (+3) <*> pure 5 = pure 8
-- Interchange: u <*> pure y == pure ($ y) <*> u
--   - Kolejność aplikowania funkcji do wartości
--   - Przykład: [(*2), (+3)] <*> pure 5 = pure ($ 5) <*> [(*2), (+3)] = [10, 8]
-- Composition: pure (.) <*> u <*> v <*> w == u <*> (v <*> w)
--   - Kompozycja funkcji w kontekście aplikatywnym

data User = User String Int
    deriving (Show)
createUser :: String -> Int -> User
createUser = User

-- Tworzenie użytkownika z wartościami Maybe - użyteczne gdy dane mogą być niepełne
-- Przykład: maybeCreateUser (Just "Jan") (Just 30) = Just (User "Jan" 30)
-- Przykład: maybeCreateUser Nothing (Just 30) = Nothing
maybeCreateUser :: Maybe String -> Maybe Int -> Maybe User
maybeCreateUser = liftA2 createUser

-- Tworzenie wielu użytkowników z list imion i wieku
-- Przykład: createMultipleUsers ["Jan", "Anna"] [30, 25] = [User "Jan" 30, User "Anna" 25]
createMultipleUsers :: [String] -> [Int] -> [User]
createMultipleUsers = liftA2 createUser

-- Monady
--
class (Functor' m, Applicative' m) => Monad' m where
    return' :: a -> m a -- to samo co pure z instancji Applicative
    bind' :: m a -> ( a -> m b) -> m b -- zamiast bind piszemy >>= w notacji infixowej
    -- join' :: m (m a) -> m a
    -- join dawał nam składanie w kategorii Kleisli
    -- f : a -> m b >=> g : b -> m c = join . g . f

-- Monada umożliwia sekwencyjne wykonywanie operacji w kontekście.
-- Operator >>= (bind) pozwala na "wyciągnięcie" wartości z kontekstu,
-- wykonanie na niej operacji i "zapakowanie" wyniku z powrotem do kontekstu.

toList :: a -> [a]
toList = pure

-- Funkcja obliczająca pierwiastek, która może się nie powieść dla liczb ujemnych
-- Przykład: fun 16 = Just 4.0
-- Przykład: fun (-4) = Nothing
fun :: Float -> Maybe Float
fun n
    | n >= 0 = Just $ sqrt n
    | otherwise = Nothing

-- Przykładowa funkcja kwadratowa zawsze się powiedzie
-- Przykład: g 3 = Just 9
g :: Float -> Maybe Float
g n = Just $ n * n

instance Monad Maybe' where
    -- >>= : Maybe a -> ( a -> Maybe b) -> Maybe b
    Nothing' >>= _ = Nothing'
    (Just' x) >>= f = f x

-- Przykłady użycia monady Maybe':
-- Just' 16 >>= fun = Just' 4.0
-- Just' (-4) >>= fun = Nothing'
-- Nothing' >>= fun = Nothing'
-- Just' 3 >>= fun >>= g = Just' 9.0

-- dla f :: a -> Maybe b , g :: b -> Maybe c
-- f >=> g :: a -> Maybe c
--
-- (f >=> g) x = case (f x) of
--  Nothing -> Nothing
--  Just y  -> g y

-- Bezpieczna wersja funkcji head, która zwraca Nothing dla pustej listy
-- Przykład: head' [1,2,3] = Just 1
-- Przykład: head' [] = Nothing
head' :: [a] -> Maybe a
head' [] = Nothing
head' (x : _) = Just x

-- Bezpieczna wersja funkcji tail, która zwraca Nothing dla pustej listy
-- Przykład: tail' [1,2,3] = Just [2,3]
-- Przykład: tail' [] = Nothing
tail' :: [a] -> Maybe [a]
tail' [] = Nothing
tail' (_ : xs) = Just xs

-- Funkcja wyciągająca trzeci element listy, używająca operatora >=>
-- Przykład: third [1,2,3,4] = Just 3
-- Przykład: third [1,2] = Nothing
third = tail' >=> tail' >=> head'

-- To samo co third, ale używając operatora >>=
-- Przykład: third_bind [1,2,3,4] = Just 3
third_bind list = (tail' list >>= tail') >>= head'

-- Notacja do:
--tail' xs >>= \ys -> head' ys
-- do 

-- To samo co third, ale używając składni do
-- Składnia do jest bardziej czytelna dla złożonych operacji monadycznych
-- Przykład: third_do [1,2,3,4] = Just 3
third_do list = do 
  ys <- tail' list  -- wyciągnij ogon listy
  zs <- tail' ys    -- wyciągnij ogon ogona
  head' zs          -- weź głowę podwójnego ogona

