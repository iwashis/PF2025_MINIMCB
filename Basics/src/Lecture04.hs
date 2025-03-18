{-# LANGUAGE DeriveFunctor #-}
module Lecture04 where
import Prelude hiding ((>>=))
import Control.Monad ((>=>), (>>=))


-- monada - funktor T: C -> C wyposazony w strukture 
-- eta : x -> T x 
-- mu : T T x -> T x 
-- + prawa monadyczne 
--

-- aplicative - funktor F : C -> C 
-- nie chcemy definiowac tego formalnie w jezyku teorii kategorii i zdefiniujemy tylko 
-- haskellowo 
--
-- pure : x -> F x  -- to samo co eta w monadzie 
-- (<*>) :: f (a -> b) -> f a -> f b 
--  + prawa aplikatywne 

-- Tw. jesli funktor F ma strukture monadyczna, to ona indukuje nturalna strukture applicative. 


-- Przyklad 1 
--
data Maybe' a = Just' a | Nothing'
  deriving (Show,Functor)

instance Applicative Maybe' where 
  pure = Just'

  Nothing' <*> _ = Nothing' 
  _ <*> Nothing' = Nothing' 
  (Just' f) <*> (Just' x) = Just' (f x)

-- typ izomorficzny z [a]
data MyList a = Nil | Con a (MyList a)
  deriving (Show,Functor)

concat' :: MyList a -> MyList a -> MyList  a
concat' Nil list2 = list2 
concat' (Con x xs) list2 = Con x (concat' xs list2)

instance Applicative MyList where 
  pure x = Con x Nil
  Nil <*> _ = Nil 
  _ <*> Nil = Nil
  (Con f fs) <*> args = fmap f args `concat'` (fs <*> args)


-- moglibysmy zdefiniowac <*> w notacji listowej jako:
-- fs <*> args = [f x | (f,x) <- zip fs args]
star :: [a->b] -> [a] -> [b]
star fs args = [f x | (f,x) <- zip fs args]

-- Funktory, Applicative: podsumowanie 
-- 
-- Functor:

class Functor' f where 
  fmap' :: (a -> b) -> f a -> f b 

-- dla f : a -> b, mamy zadana F(f): Fa -> Fb 
-- F(id) = id 
-- F( f . g ) = F(f) . F(g)
--
-- Identity: fmap id == id
-- Composition: fmap (g . h) == (fmap g) . (fmap h)

class (Functor' f) => Applicative' f where 
  pure' :: a -> f a  
  -- (<*>) :: f (a -> b) -> f a -> f b
  liftA2' :: (a -> b -> c) -> f a -> f b -> f c     
-- 
--
-- Identity: pure id <*> v == v
-- Homomorphism: pure f <*> pure x == pure (f x)
-- Interchange: u <*> pure y == pure ($ y) <*> u
-- Composition: pure (.) <*> u <*> v <*> w == u <*> (v <*> w)

data User = User String Int 
  deriving Show

createUser :: String -> Int -> User
createUser = User

maybeCreateUser :: Maybe String -> Maybe Int -> Maybe User
maybeCreateUser = liftA2 createUser

createMultipleUsers:: [String] -> [Int] -> [User]
createMultipleUsers = liftA2 createUser



-- 
-- Monady
--
class (Functor' m, Applicative' m) => Monad' m where  
  return' :: a -> m a -- to samo co pure z instancji Applicative
  -- (>>=) :: m a -> ( a -> m b) -> m b
  -- join' :: m (m a) -> m a
  -- join dawal nam skladanie w kategorii Kleisli 
  -- f : a -> m b >=> g : b -> m c = join . g . f

toList :: a -> [a]
toList = pure 

f :: Float -> Maybe Float 
f n 
  | n >= 0 = Just $ sqrt n 
  | otherwise = Nothing

g :: Float -> Maybe Float 
g n = Just $ n*n


instance Monad Maybe' where 
  -- >>= : Maybe a -> ( a -> Maybe b) -> Maybe b 
    Nothing' >>= _ = Nothing' 
    (Just' x) >>= f = f x
  -- dla f :: a -> Maybe b , g :: b -> Maybe c 
  -- f >=> g :: a -> Maybe c 
  --
  -- (f >=> g) x = case (f x) of
  --  Nothing -> Nothing
  --  Just y  -> g y

head' :: [a] -> Maybe a
head' []     = Nothing
head' (x:_) = Just x

tail' :: [a] -> Maybe [a]
tail' []     = Nothing
tail' (_:xs) = Just xs

third = tail' >=> tail' >=> head'

third_bind list = (tail' list >>= tail') >>= head'   
