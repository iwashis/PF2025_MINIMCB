{-# LANGUAGE DeriveFunctor #-}
module Lecture05 where
import Lecture04 (g)
import Data.Bifunctor (Bifunctor(bimap))


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

exampleStrList = Con "ala" (Con "maciej" (Con "tomek" Nil))


f :: String -> Int -> (String , Int)
f str i = (str,i)

instance Monad MyList where 
  return = pure 
  -- >>= :: MyList a -> (a -> MyList b) -> MyList b
  Nil >>= _ = Nil 
  (Con x xs) >>= f = concat' (f x) (xs >>= f) 

triple = \x -> Con x (Con x (Con x Nil))

expression = do 
  x <- (Con "Bunny" Nil)
  y <- triple x
  triple y

fib = 0 : 1 : ( do
                 (x,y) <- zip fib $ tail fib
                 return $ x + y
              )

exampleList :: [String] 
exampleList = do 
  x <- ['A'..'Z']
  y <- ['a'..'z']
  return [x,y] 


-- Monada stanu: intro
-- (s x a)^s 
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
  pure x = State (\s -> (s,x))
  -- liftA2 :: (a -> b-> c) -> f a -> f b -> f c
  -- f  :: a -> b -> c 
  -- f1 :: s -> (s, a)
  -- f2 :: s -> (s, b)
  liftA2 f (State f1) (State f2) = State g -- g :: s -> (s, c) 
    where 
      g s = (s'', z) -- :: (s, c)
        where 
          x = snd $ f1 s
          s' = fst $ f1 s
          s'' = fst $ f2 s'
          y = snd $ f2 s'
          z = f x y 

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
