{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Lecture05 where


-- typ izomorficzny z [a]
data MyList a = Nil | Con a (MyList a)
    deriving (Show, Functor)

concat' :: MyList a -> MyList a -> MyList a
concat' Nil list2 = list2
concat' (Con x xs) list2 = Con x (concat' xs list2)

instance Applicative MyList where
    pure x = Con x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Con f fs) <*> args = fmap f args `concat'` (fs <*> args)

exampleIntList :: MyList Int
exampleIntList = Con 1 (Con 2 Nil)

exampleStrList :: MyList String
exampleStrList = Con "ala" (Con "maciej" (Con "tomek" Nil))


pair :: String -> Int -> (String , Int)
pair str i = (str,i)

instance Monad MyList where 
  return = pure 
  -- >>= :: MyList a -> (a -> MyList b) -> MyList b
  Nil >>= _ = Nil 
  (Con x xs) >>= f = concat' (f x) (xs >>= f) 

triple :: a -> MyList a
triple x = Con x (Con x (Con x Nil))

expression :: MyList String
expression = do 
  x <- Con "Bunny" Nil
  y <- triple x
  triple y


fib :: [Integer]
fib = 0 : 1 : ( do
                 (x,y) <- zip fib $ tail fib
                 return $ x + y
              )

exampleList :: [String] 
exampleList = do 
  x <- ['A'..'Z']
  y <- ['a'..'z']
  return [x,y] 

----------------------------------
-- Monada stanu: Wprowadzenie
-- (s x a)^s 
----------------------------------

newtype State s a = State { runState :: s -> (s,a)}

-- rownowaznie mozemy tez:
-- newtype State s a = State (s -> (s,a)) 
-- runState :: State s a -> (s -> (s,a))
-- runState (State f) = f

instance Functor (State s) where 
  -- ( a -> b) -> State s a -> State s b
  fmap f state = State $ (\(s,x) -> (s, f x)) . runState state 
    -- s ---runState state --> (s,a) ---- \(s,x) -> (s, f x) ---> (s,b)

instance Applicative (State s) where 
  -- pure :: a -> State s a 
  pure x = State (,x) -- funkcja (,x) to to samo co \s -> (s,x)
  -- Teraz zdefiniujemy liftA2, ktorej typ w ogolnosci jest nastepujacy
  -- liftA2 :: (a -> b-> c) -> f a -> f b -> f c
  liftA2 f (State f1) (State f2) = State g -- g :: s -> (s, c) 
  -- f  :: a -> b -> c 
  -- f1 :: s -> (s, a)
  -- f2 :: s -> (s, b)
    where 
      g s = let 
              (s' , x) = f1 s
              (s'', y) = f2 s'
              z = f x y 
              in (s'', z) -- :: (s, c)
          

instance Monad (State s) where 
  -- state :: State s a, runState state :: s -> (s,a) 
  -- f :: a -> State s b -- intuicyjnie mozemy traktowac jako a -> (s -> (s,b)) 
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
