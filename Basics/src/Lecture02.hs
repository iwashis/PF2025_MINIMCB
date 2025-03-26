{-# LANGUAGE RecordWildCards #-}
module Lecture02 where

-- Haskell ma wbudowane typy danych, np. Bool:
-- data Bool = True | False

-- Definiujemy własny typ wyliczeniowy dla podstawowych kolorów
-- BasicColors ma trzy wartości konstruktora: Red, Blue, Green
-- Przykład użycia: kolor = Red
data BasicColors = Red | Blue | Green

-- Definiujemy typ danych Shape (Kształt) z trzema różnymi konstruktorami
-- Rectangle przyjmuje dwie pary współrzędnych (lewy dolny róg i prawy górny róg)
-- Circle przyjmuje parę współrzędnych środka i promień
-- Point przyjmuje parę współrzędnych punktu
-- Przykład użycia: 
-- kwadrat = Rectangle (0,0) (5,5)
-- kolo = Circle (3,3) 2
-- punkt = Point (1,1)
data Shape
    = Rectangle (Double, Double) (Double, Double)
    | Circle (Double, Double) Double
    | Point (Double, Double)

-- Funkcja obliczająca pole powierzchni kształtu
-- Przykłady:
-- volume (Point (1,1)) = 0
-- volume (Rectangle (0,0) (3,4)) = 12.0
volume :: Shape -> Double
volume (Point (_, _)) = 0
volume (Circle _ r) = pi * (r ^ 2)
volume (Rectangle (x, y) (z, t)) = abs (z - x) * abs (t - y)

-- Funkcja zwiększająca promień koła o 5, jeśli kształt jest kołem
-- W przeciwnym razie zwraca oryginalny kształt
-- Przykłady:
-- changeRadiusIfCircle (Circle (0,0) 2) = Circle (0,0) 7
-- changeRadiusIfCircle (Rectangle (0,0) (3,4)) = Rectangle (0,0) (3,4)
changeRadiusIfCircle :: Shape -> Shape
changeRadiusIfCircle (Circle point r) = Circle point (r + 5)
changeRadiusIfCircle x = x

-- Definiujemy prosty typ danych dla osoby
-- Składa się z imienia, nazwiska i wieku
-- Przykład: osoba = Person "Jan" "Kowalski" 30
data Person = Person String String Int
    deriving (Show)

p = Person "Tomek" "Kowalski" 25

-- Definiujemy typ danych dla osoby używając rekordów
-- Rekordy automatycznie tworzą funkcje dostępu do pól
-- Przykład: 
-- osoba2 = Person2 "Jan" "Kowalski" 30
-- name osoba2 = "Jan"
-- surname osoba2 = "Kowalski"
-- age osoba2 = 30
data Person2 = Person2
    { name :: String
    , surname :: String
    , age :: Int
    }
    deriving (Show)

p2 = Person2 "Tomek" "Kowalski" 25

-- Funkcja dodająca przedrostek "Mr." do imienia osoby
-- Jeśli nazwisko jest puste, nie modyfikuje osoby
-- Używa składni RecordWildCards do ułatwienia pracy z rekordami
-- Przykłady:
-- addMr (Person2 "Jan" "Kowalski" 30) = Person2 {name = "Mr.Jan", surname = "Kowalski", age = 30}
-- addMr (Person2 "Jan" "" 30) = Person2 {name = "Jan", surname = "", age = 30}
addMr :: Person2 -> Person2
addMr p@Person2{name = name, surname = surname, ..} =
    if surname /= ""
        then Person2{name = "Mr." ++ name, ..}
        else p

-- Definiujemy własną listę liczb całkowitych
-- EmptyIntList oznacza pustą listę
-- NonEmptyList przechowuje wartość i resztę listy
-- Przykład: lista = NonEmptyList 1 (NonEmptyList 2 (NonEmptyList 3 EmptyIntList))
data IntList = EmptyIntList | NonEmptyList Int IntList
    deriving (Show) -- Eq)

listExample = NonEmptyList 4 (NonEmptyList 5 EmptyIntList)

-- Funkcja obliczająca długość listy IntList
-- Przykłady:
-- length2 EmptyIntList = 0
-- length2 (NonEmptyList 1 (NonEmptyList 2 EmptyIntList)) = 2
length2 :: IntList -> Int
length2 EmptyIntList = 0
length2 (NonEmptyList _ list) = 1 + length2 list

-- Definiujemy drzewo binarne dla liczb całkowitych
-- EmptyTree oznacza puste drzewo
-- IntNode przechowuje wartość i lewe oraz prawe poddrzewo
-- Przykład: 
-- drzewo = IntNode 5 (IntNode 3 EmptyTree EmptyTree) (IntNode 7 EmptyTree EmptyTree)
data IntTree = EmptyTree | IntNode Int IntTree IntTree

-- Definiujemy ogólne drzewo binarne dla dowolnego typu
-- Empty oznacza puste drzewo
-- Node przechowuje wartość typu a i lewe oraz prawe poddrzewo
-- Przykład:
-- drzewoInt = Node 5 (Node 3 Empty Empty) (Node 7 Empty Empty)
-- drzewoStr = Node "root" (Node "left" Empty Empty) (Node "right" Empty Empty)
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

-- Przykład drzewa przechowującego listy liczb całkowitych
exampleTree :: Tree [Int]
exampleTree = Node [1, 2] Empty (Node [4] Empty Empty)

-- Definiujemy ogólną listę dla dowolnego typu
-- EmptyList oznacza pustą listę
-- Head przechowuje wartość typu a i resztę listy
-- Przykład:
-- listaInt = Head 1 (Head 2 (Head 3 EmptyList))
-- listaStr = Head "a" (Head "b" (Head "c" EmptyList))
data List a = EmptyList | Head a (List a)
-- deriving Eq
--  deriving Show

-- TODO: Poprawić definicję instancji Show. Powód: teraz jest brzydko
-- Własna implementacja Show dla List
-- Przykład:
-- show (Head 1 (Head 2 EmptyList)) = "1,2,"
instance (Show a) => Show (List a) where
    show EmptyList = ""
    show (Head a list) = show a ++ "," ++ show list

-- Własna implementacja Eq dla List
-- Dwie listy są równe, jeśli mają takie same elementy w tej samej kolejności
-- Przykłady:
-- Head 1 (Head 2 EmptyList) == Head 1 (Head 2 EmptyList) = True
-- Head 1 (Head 2 EmptyList) == Head 1 (Head 3 EmptyList) = False
instance (Eq a) => Eq (List a) where
    EmptyList == EmptyList = True
    (Head a _) == EmptyList = False
    EmptyList == (Head a _) = False
    (Head a list1) == (Head b list2) = a == b && list1 == list2

-- Przykłady list
exampleList :: List Int
exampleList = Head 1 (Head 50 EmptyList) -- 1,50,

exampleList2 :: List Int
exampleList2 = Head 3 (Head 50 EmptyList) -- 3,50,

-- Definicja typu z dwoma różnymi konstruktorami o tych samych polach
-- Przykład:
-- para1 = Pair "klucz" 5
-- para2 = Pair2 "klucz" 5
data PairOrPair a b = Pair a b | Pair2 a b

-- Aliasy typów - pozwalają tworzyć nowe nazwy dla istniejących typów
-- Przykład: x :: NewInt = 5
type NewInt = Int

-- Aliasy typów poprawiające czytelność kodu
type Width = Int
type Height = Int

-- Funkcja obliczająca pole prostokąta
-- Przykład: volume2 3 4 = 12
volume2 :: Width -> Height -> Int
volume2 h w = h * w

-- Funktory
-- f :: a -> b
-- F(f) = F a -> F b -- ta funkcja ma być "dobra"
-- czyli: F(f . g) == F (f) . F(g) <-

-- Implementacja Functor dla List
-- Pozwala na mapowanie funkcji na wszystkie elementy listy
-- Przykłady:
-- fmap (+1) (Head 1 (Head 2 EmptyList)) = Head 2 (Head 3 EmptyList)
-- fmap show (Head 1 (Head 2 EmptyList)) = Head "1" (Head "2" EmptyList)
instance Functor List where
    -- fmap :: ( a -> b ) -> ( List a -> List b)
    fmap _ EmptyList = EmptyList
    fmap f (Head a list) = Head (f a) (fmap f list)

-- Implementacja Semigroup dla List mogłaby wyglądać tak:
instance Semigroup (List a) where
    -- (<>) :: List a -> List a -> List a
    EmptyList <> ys = ys
    (Head x xs) <> ys = Head x (xs <> ys)

-- Przykład:
-- Head 1 (Head 2 EmptyList) <> Head 3 (Head 4 EmptyList) = Head 1 (Head 2 (Head 3 (Head 4 EmptyList)))

-- Implementacja Monoid dla List mogłaby wyglądać tak:
instance Monoid (List a) where
    -- mempty :: List a
    mempty = EmptyList
    -- mappend :: List a -> List a -> List a
    mappend = (<>) -- tego pisac nie musimy, bez tej linii kompilator wie, ze mappend w domysle definiowane jest przez operacje polgrupowa
--
-- Przykład:
-- mempty <> Head 1 (Head 2 EmptyList) = Head 1 (Head 2 EmptyList)
-- Head 1 (Head 2 EmptyList) <> mempty = Head 1 (Head 2 EmptyList)
