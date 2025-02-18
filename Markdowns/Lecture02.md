# Programowanie funkcyjne

## Tomasz Brengos

Wykład 2


## Kod wykładu 
Basics/Lecture02.hs


---

# Algebraic Data Types

Niehaskellowe podsumowanie gramatyki ADT:
```
τ =  τ in BasicTypes | τ x τ | τ + τ | τ -> τ |  τ :=  τ   
```

---

# Enumeration Types

```haskell
data Bool  = True | False

data BasicColours = Red
                    | Green
                    | Blue
```

Ogólnie:

```haskell
data EnumType  = EnumType_1 | EnumType_2 | ... | EnumType_n
```

---

# Bardziej zaawansowane ADTs:

```haskell 
data Type = Cons1 Type_11 ... Type_1n1 
          | Cons2 Type_21 ... Type_2n2
          |  ...                        
          | Consm Type_n1 ... Type_mnm
```

Przykład
```haskell
data Shape = Rectangle (Double,Double) (Double,Double) 
           | Circle (Double, Double) Double     
           | Point (Double, Double)

exampleRect :: Shape
exampleRect = Rectangle (0,1) (2,4)

exampleCirc :: Shape
exampleCirc = Circle (0,0) 5

changeRadiusIfCircle :: Shape -> Shape
changeRadiusIfCircle (Circle point r) = Circle point (r+5)
changeRadiusIfCircle x = x
```

---

# Można też inaczej...

Dokładne definiowanie typów. Można tak: 
```haskell 
data Person1 = Person1 String String String Integer 
  deriving Show
```
ale też można tak:
```haskell
data Person2 = Person2 { name    :: String
                       , surname :: String
                       , address :: String 
                       , age     :: Integer
                       }
  deriving Show --deriving (Eq, Show)
```
Rozważmy trzy przykłady:
```haskell
examplePerson1 = Person1 "Tomasz" "Kowalski" "Warszawa" 20
examplePerson2 = Person2 "Tomasz" "Kowalski" "Warszawa" 20
examplePerson3 = Person2 { surname = "Kowalski"
                         , name    = "Tomasz"
                         , age     = 20
                         , address = "Warszawa" 
                         }
```

---

# Praca z ADT

```haskell
data Person2 = Person2 { name    :: String
                       , surname :: String
                       , address :: String 
                       , age     :: Integer
                       }
  deriving deriving (Eq, Show)
```
Rozważmy funkcję
```haskell 
addMr :: Person2 -> Person2
addMr p@Person2 { name = name, surname = surname, .. }  =
   if surname /= "" then Person2 { name = "Mr." ++ name, .. }
                    else p 

```

Uwaga! Powyższy kod działa, jeśli na początku pliku
doda się:
```haskell
{-# LANGUAGE RecordWildCards #-}
```

---

# Rekurencyjne ADT

```haskell
data IntList = EmptyList | Head Int IntList

data IntTree = EmptyTree | Node Int IntTree IntTree
```
Jak pracować z rekurencyjnymi typami danych?
```haskell
length :: IntList -> Int
...
```

---

# Typy parametryczne

```haskell
data List a    = EmptyList | Head a (List a)
```
Porównajmy z:
```haskell
data [a]       = []        | a:[a]
```
Drzewo binarne o wewnętrznych wartościach typu a:
```haskell
data BinTree a = EmptyTree | Node a (BinTree a) (BinTree a)
```
i jeszcze przykłady:
```haskell
data Tuple a      = Tuple a a

data Triple a b c = Triple a b c

data PairOrMap a b   = Pair a b | Map (a -> b)
```

---

# Ważne przykłady

```haskell
data ()          = ()
data Bool        = True   | False
data Maybe a     = Just a | Nothing
data Either a b  = Left a | Right b
data [a]         = []     |  a:[a]
data State s a   = State { runState :: s -> (a,s) }
```


