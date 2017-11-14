module Lib where

import Data.List ( partition )

someFunc :: IO ()
someFunc = putStrLn "hello"

data Tree a = Node a (Tree a) (Tree a) | Empty deriving ( Show, Eq )

size :: Tree a -> Int
size Empty = 0
size (Node _ l r) = 1 + size l + size r

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

insert :: (Ord a, Eq a) => a -> Tree a -> Tree a
insert a Empty = Node a Empty Empty
insert a o@(Node x l r)
  | a == x    = o
  | a < x     = Node x (insert a l) r
  | otherwise = Node x l (insert a r)

contains :: (Ord a, Eq a) => a -> Tree a -> Bool
contains _ Empty = False
contains v (Node x l r)
  | x == v = True
  | v < x  = contains v l
  | v > x  = contains v r

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort sm ++ [x] ++ qsort lg
  where
    (sm, lg) = partition (<x) xs
