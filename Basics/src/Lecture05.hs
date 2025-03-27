{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Lecture05 (State(..)) where

import Lecture04 (MyList(..), concat')
-- Nie musimy na nowo definicjowac MyList, bo pisalismy odpowiednie definicje 
-- w trakcie wykladu 4. Jedyne co robimy to importujemy najwazniejsze funkcjonalnosci 
-- powyzej. 
--
-- Przypomnienie czym jest MyList w komentarzu:
-- -- typ izomorficzny z [a]
-- -- MyList to własna implementacja listy, która ma tę samą strukturę co standardowa lista w Haskellu
-- data MyList a = Nil | Con a (MyList a)
--     deriving (Show, Functor)
--
-- -- concat' łączy dwie listy typu MyList
-- -- Przykład: concat' (Con 1 (Con 2 Nil)) (Con 3 Nil) = Con 1 (Con 2 (Con 3 Nil))
-- -- Jest to odpowiednik operatora (++) dla standardowych list
-- concat' :: MyList a -> MyList a -> MyList a
-- concat' Nil list2 = list2
-- concat' (Con x xs) list2 = Con x (concat' xs list2)

-- instance Applicative MyList where
--     -- pure tworzy listę zawierającą tylko jeden element
--     -- Przykład: pure 5 = Con 5 Nil
--     pure x = Con x Nil
--
--     -- Implementacja operatora <*> dla MyList
--     -- Przykład: Con (+1) (Con (*2) Nil) <*> Con 3 (Con 4 Nil) = Con 4 (Con 5 (Con 6 (Con 8 Nil)))
--     Nil <*> _ = Nil
--     _ <*> Nil = Nil
--     (Con f fs) <*> args = fmap f args `concat'` (fs <*> args)
--
-- Przykładowa lista liczb całkowitych
-- exampleIntList = [1, 2]
exampleIntList :: MyList Int
exampleIntList = Con 1 (Con 2 Nil)

-- Przykładowa lista napisów
-- exampleStrList = ["ala", "maciej", "tomek"]
exampleStrList :: MyList String
exampleStrList = Con "ala" (Con "maciej" (Con "tomek" Nil))

-- Funkcja tworząca parę ze String i Int
-- Przykład: pair "abc" 123 = ("abc", 123)
pair :: String -> Int -> (String , Int)
pair str i = (str,i)
-- rozwazyc liftA2 pair exampleStrList exampleIntList


instance Monad MyList where 
  -- return to to samo co pure z instancji Applicative
  return = pure 
  
  -- Implementacja operatora >>= (bind) dla MyList
  -- >>= :: MyList a -> (a -> MyList b) -> MyList b
  -- Przykład: Con 1 (Con 2 Nil) >>= \x -> Con (x+1) (Con (x*2) Nil)
  --         = Con 2 (Con 2 (Con 3 (Con 4 Nil)))
  Nil >>= _ = Nil 
  (Con x xs) >>= f = concat' (f x) (xs >>= f) 

-- Funkcja tworząca listę zawierającą trzy kopie podanego elementu
-- Przykład: triple 5 = Con 5 (Con 5 (Con 5 Nil))
-- Można myśleć o niej jako o funkcji, która "potrajania" wartość w kontekście MyList
triple :: a -> MyList a
triple x = Con x (Con x (Con x Nil))

-- Wyrażenie wykorzystujące notację do dla monady MyList
-- Wynik: Con "BunnyBunnyBunny" (Con "BunnyBunnyBunny" (Con "BunnyBunnyBunny" Nil))
-- To wyrażenie:
-- 1. Tworzy listę z jednym elementem "Bunny"
-- 2. Tworzy listę z trzema kopiami każdego elementu poprzedniej listy (czyli "Bunny")
-- 3. Dla każdego z tych elementów tworzy listę z trzema jego kopiami
expression :: MyList String
expression = do 
  x <- Con "Bunny" Nil
  y <- triple x
  triple y

-- Definicja ciągu Fibonacciego jako nieskończonej listy
-- Wykorzystuje zapętlenie i operację zip do generowania kolejnych elementów
-- Przykład: take 10 fib = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
fib :: [Integer]
fib = 0 : 1 : ( do
                 (x,y) <- zip fib $ tail fib
                 return $ x + y
              )

-- Lista wszystkich możliwych par liter: duża litera + mała litera
-- Przykład: take 5 exampleList = ["Aa", "Ab", "Ac", "Ad", "Ae"]
-- Rozmiar tej listy to 26 * 26 = 676 elementów
exampleList :: [String] 
exampleList = do 
  x <- ['A'..'Z']
  y <- ['a'..'z']
  return [x,y] 

----------------------------------
-- Monada stanu: Wprowadzenie
-- (s x a)^s 
----------------------------------

-- Monada Stan (State) pozwala na modelowanie obliczeń, które mają dostęp do pewnego stanu,
-- mogą go modyfikować i przekazywać dalej przez obliczenie.
-- s - typ stanu
-- a - typ wyniku
newtype State s a = State { runState :: s -> (s,a)}
-- równoważnie możemy też:
-- newtype State s a = State (s -> (s,a)) 
-- runState :: State s a -> (s -> (s,a))
-- runState (State f) = f

-- Implementacja funktora dla monady State
instance Functor (State s) where 
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f state = State $ (\(s,x) -> (s, f x)) . runState state 
    -- s ---runState state --> (s,a) ---- \(s,x) -> (s, f x) ---> (s,b)
    -- Działanie: uruchamiamy state, dostajemy parę (stan, wynik),
    -- zachowujemy stan i aplikujemy funkcję f do wyniku

-- Implementacja aplikatora dla monady State
instance Applicative (State s) where 
  -- pure :: a -> State s a 
  -- Tworzy obliczenie stanowe, które nie zmienia stanu i zwraca wartość x
  pure x = State (,x) -- funkcja (,x) to to samo co \s -> (s,x)
  
  -- Implementacja liftA2 dla State
  -- Teraz zdefiniujemy liftA2, której typ w ogólności jest następujący
  -- liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 f (State f1) (State f2) = State g -- g :: s -> (s, c) 
  -- f  :: a -> b -> c 
  -- f1 :: s -> (s, a)
  -- f2 :: s -> (s, b)
    where 
      g s = let 
              (s' , x) = f1 s     -- Uruchamiamy pierwsze obliczenie, dostajemy nowy stan s' i wynik x
              (s'', y) = f2 s'    -- Uruchamiamy drugie obliczenie z nowym stanem, dostajemy s'' i y
              z = f x y           -- Łączymy wyniki obliczeń za pomocą funkcji f
              in (s'', z)         -- :: (s, c)
          
-- Implementacja monady dla State
instance Monad (State s) where 
  -- state :: State s a, runState state :: s -> (s,a) 
  -- f :: a -> State s b -- intuicyjnie możemy traktować jako a -> (s -> (s,b)) 
  --                                                          a -> s -> (s,b)
  --                                                          (a,s) -> (s,b)
  --  f'                                                      (s,a) -> (s,b)
  -- wynik :: State s b -- intuicyjnie s -> (s,b)
  -- state >>= f = State $ f' . runState state
  --   where 
  --     f' (st,x) = runState (f x) st 
  --                     -- f x :: State s b  
                      -- runState (f x) :: s -> (s,b)
  state >>= f = State $ ( \(st,x) -> runState (f x) st ) . runState state
