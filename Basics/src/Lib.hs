module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someOtherFunc"

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort le ++ [x] ++ quicksort gr 
  where
    le = filter ( < x) xs 
    gr = filter ( >= x) xs


add :: Int -> (Int -> Int) 
add x = \y -> x + y
-- add x y =  x + y
--
t = add 6 

ones = 1:ones

fib = 0:1:[ x + y | (x,y) <- zip fib (drop 1 fib) ] -- [0,1]


sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs


