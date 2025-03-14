{-# LANGUAGE DeriveFunctor #-}
module Lecture04 where



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
 
