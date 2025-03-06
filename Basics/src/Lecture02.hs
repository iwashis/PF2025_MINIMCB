{-# LANGUAGE RecordWildCards #-}

module Lecture02 where

-- data Bool = True | False

data BasicColors = Red | Blue | Green

data Shape
    = Rectangle (Double, Double) (Double, Double)
    | Circle (Double, Double) Double
    | Point (Double, Double)

volume :: Shape -> Double
volume (Point (_, _)) = 0
volume (Circle (x, y) r) = pi * (r ^ 2)
volume (Rectangle (x, y) (z, t)) = abs (z - x) * abs (t - y)

changeRadiusIfCircle :: Shape -> Shape
changeRadiusIfCircle (Circle point r) = Circle point (r + 5)
changeRadiusIfCircle x = x

data Person = Person String String Int
    deriving (Show)

p = Person "Tomek" "Kowalski" 25

data Person2 = Person2
    { name :: String
    , surname :: String
    , age :: Int
    }
    deriving (Show)

p2 = Person2 "Tomek" "Kowalski" 25

addMr :: Person2 -> Person2
addMr p@Person2{name = name, surname = surname, ..} =
    if surname /= ""
        then Person2{name = "Mr." ++ name, ..}
        else p

data IntList = EmptyIntList | NonEmptyList Int IntList
    deriving (Show) -- Eq)

listExample = NonEmptyList 4 (NonEmptyList 5 EmptyIntList)

length2 :: IntList -> Int
length2 EmptyIntList = 0
length2 (NonEmptyList _ list) = 1 + length2 list

data IntTree = EmptyTree | IntNode Int IntTree IntTree

data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Show)

exampleTree :: Tree [Int]
exampleTree = Node [1, 2] Empty (Node [4] Empty Empty)

data List a = EmptyList | Head a (List a)

-- deriving Eq
--  deriving Show

-- TODO: Poprawić definicję instancji Show. Powód: teraz jest brzydko
instance (Show a) => Show (List a) where
    show EmptyList = ""
    show (Head a list) = show a ++ "," ++ show list

instance (Eq a) => Eq (List a) where
    EmptyList == EmptyList = True
    (Head _ _) == EmptyList = False
    EmptyList == (Head _ _) = False
    (Head a list1) == (Head b list2) = a == b && list1 == list2

exampleList :: List Int
exampleList = Head 1 (Head 50 EmptyList) -- 1,50

exampleList2 :: List Int
exampleList2 = Head 3 (Head 50 EmptyList) -- 1,50

data PairOrPair a b = Pair a b | Pair2 a b
type NewInt = Int

type Width = Int
type Height = Int
volume2 :: Width -> Height -> Int
volume2 h w = h * w

-- Funktory
-- f :: a -> b
-- F(f) = F a -> F b -- ta funkcja ma byc "dobra"
-- czyli: F(f . g) == F (f) . F(g) <-

instance Functor List where
    -- fmap :: ( a -> b ) -> ( List a -> List b)
    fmap _ EmptyList = EmptyList
    fmap f (Head a list) = Head (f a) (fmap f list)

-- TODO: dokończyć definicję instancji
instance Semigroup (List a)

instance Monoid (List a)
