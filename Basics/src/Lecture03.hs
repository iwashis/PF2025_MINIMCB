module Lecture03 where

import qualified Data.Monoid as M

 -- foldl (#) seed [a1..an] -> ((..(seed#a1)#a2#..)#an
 -- foldr (*) seed [a1..an] -> a1*(a2*..(an*seed))..)
 --

--foldl :: (b -> a -> b) -> b -> [a] -> b
initl :: [a] -> [[a]]
initl list = foldl f seed list  
  where
    seed = [[]]
    f s c = s ++ [last s ++ [c] ]

-- TODO: napisac init uzywajac foldr
--foldr :: (a -> b -> b) -> b -> [a] -> b    
-- foldr (*) seed [a1..an] -> a1*(a2*..(an*seed))..)
--


data Tree a = EmptyTree | Leaf a | Node a (Tree a) (Tree a)


tree :: Tree String
tree = Node "a" (Node "b" EmptyTree (Leaf "c")) (Node "d" EmptyTree EmptyTree)

tree2 :: Tree Int 
tree2 = Node 1 (Node 5 EmptyTree (Leaf 7)) (Node 3 EmptyTree EmptyTree)



instance Foldable Tree where  
  foldMap _ EmptyTree = M.mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node x left right) = foldMap f left `M.mappend` f x `M.mappend` foldMap f right 

  foldr _ seed EmptyTree           = seed
  foldr f seed (Leaf x)            = f x seed
  foldr f seed (Node x left right) = foldr f (f x (foldr f seed right)) left
