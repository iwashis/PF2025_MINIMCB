# Programowanie funkcyjne

## Tomasz Brengos

Wykład 6


## Kod wykładu 
Basics/Lecture05.hs -- kończymy pisanie State1 s a
Basics/Lecture06.hs

---

# State1 s a: jeszcze kilka słów

## Przypomnienie z TK
Mamy następującą jednoznaczną odpowiedniość
```haskell
x -> State s y  ===  x-> s -> (y,s) === (x,s) -> (y,s)
```

# Składanie w Kleisli dla State s y
```haskell
f :: x -> State s y >=> g :: y -> State s z   
                   === 
g" :: (y,s)-> (z,s) . f" :: (x,s) -> (y,s)
```

# Operacja bind i jej interpretacja
```haskell
st :: State s x >>= f :: x -> State s y
                ===
f" :: (x,s) -> (y,s)  .  st :: s -> (x,s)  
```

---


# Dodatkowa funkcjonalność monady State

```haskell
get :: State s s            -- \state   -> (state, state)
put :: s -> State s ()      -- \st1 st2 -> ((), st1) 
```
Dokończyć w pliku Lecture05!

---

# Zadanie 1

```haskell
type Cost = State Int
```

Zadanie: Jak interpretować następujące typy?
```haskell
Double -> Cost Double
Cost String
```

## Zadanie
Rozważmy listę [f1,...,fn] funkcji fi :: Double -> Double. Załóżmy, że 
koszt policzenia wartości dowolnej funkcji powyższego typu dla ujemnych argumentów 
równy jest -3, dla nieujemnych 2. Wielokrotne liczenie wartości funkcji powoduje zsumowanie 
kosztów. Napisać funkcje:
```haskell
addCost :: (Double -> Double) -> Double -> Cost Double
showCostValue :: [Double -> Double] -> Double -> (Int, Double)
```

---

# Diagramy sznurkowe dla State monad

Ogólnie, jeśli mamy wyrażenie f :: State s a zadane przez
```haskell
f = f1 >> f2 >> f3 >> ... >> fn
```

lub, równoważnie, zapisane w notacji do:
```haskell
f = do
  f1
  f2
  ...
  fn
```

możemy je rozumieć używając notacji diagramów sznurkowych:
```haskell
 s |-----| s        |-----|  s        |------| s
---|     |----------|     |----...----|      |-----
   | f1  |          | f2  |           |  fn  |
   |     |--        |     |---        |      |-----
   |_____| a1       |_____|  a2       |______| an = a
```

Jeśli zaś nasze wyrażenie wygląda następująco:
```haskell
f = do
  f1
  ...
  x <- fi
  ...
  fj x
  ...
  fn
```
to diagram sznurkowy odpowiadający f wygląda tak:
```haskell

 s |-----| s        |-----|  s     |------|            |------|
---|     |---... ---|     |--------|      |--...-------|      |----...
   | f1  |          | fi  |        |fi+1  |            | fj   |
   |     |--        |     |---|    |      |--       |--|      |----...
   |_____| a1       |_____|   |    |______|         |  |______|
                              |                     |
                              |_____________________|

```


---

# Jak to wygląda na przykładzie?

Rozważmy następujący kod:
```haskell
addCost :: (Double -> Double) -> Double -> State Int Double
addCost f r = do
  cost <- get
  put $ new cost
  return $ f r
  where new x = if r > 0 then x-3 else x+2
```
1. Jakiego typu jest wartość addCost f r?
Odp. State Int Double, lub, izomorficznie, Int -> (Double,Int).

Spróbujmy przeanalizować krok po kroku co się dzieje w powyższym kodzie:
```haskell

 s |-----| s              |-----|  new s |------| new s
---|     |----------------|     |--------|      |-----
   | get |                | put |        |return|
   |     |--- |new|-------|     |---     |      |-----
   |_____| s         new s|_____|  ()    |______| f r

```

---

# Zadanie 2

Praca z generatorami liczb pseudolosowych.

## Zadanie
Używając monady State Integer napisać funkcję generatePseudoRandomList
przyjmującą jako argument długość listy (typu Integer) generującą listę pseudolosowych
elementów typu Integer o zadanej długości.

Podpowiedź:
```haskell
type Random = State Integer

shuffle :: Integer -> Integer -- funkcja tasująca

randomInteger :: Random Integer
randomInteger = do   -- randomInteger = get >>= put . shuffle >> get
  x <- get
  put $ shuffle x
  return x           -- tu można po prostu get

randomList :: Integer -> Random [Integer]   -- replicateM n randomInteger
randomList 0 = return []
randomList n = do
  x  <- randomInteger
  xs <- randomList $ n-1
  return (x:xs)
```
Na diagramach sznurkowych (część) randomList wygląda następująco:
```haskell

 s |-----| shuffle s|-----|   ...     |------| shuffle^n s
---|     |----------|     |----...----|      |-----
   |     |          |     |           |      |
   |     |--        |     |---        |      |-----
   |_____| s        |_____|shuffle s  |______| shuffle^n-1 s

```

