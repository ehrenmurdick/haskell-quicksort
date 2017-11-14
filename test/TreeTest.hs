module TreeTest where

import Lib

import Data.List ( foldr
                 , nub )

import Test.QuickCheck

treeTest = do
  quickCheck (prop_functor_law_id :: (Tree Int) -> Bool)
  quickCheck (prop_size :: [Int] -> Bool)
  quickCheck (prop_sorted :: (Tree Int) -> Bool)
  quickCheck (prop_functor_law_comp :: (Tree Int) -> Bool)
  quickCheck (prop_contains :: [Int] -> Bool)

instance (Arbitrary a, Ord a) => Arbitrary (Tree a) where
  arbitrary = do
    xs <- listOf arbitrary
    return (treeify xs)

prop_sorted Empty = True
prop_sorted (Node x l r) = (x > biggest l && x < smallest r) && prop_sorted l && prop_sorted r
  where
    biggest Empty  = minBound
    biggest (Node x l r)
      | x > biggest r && x > biggest l = x
      | x > biggest r = biggest l
      | x > biggest l = biggest r
    smallest Empty = maxBound
    smallest (Node x l r)
      | x < smallest l && x < smallest r = x
      | x < smallest l = smallest r
      | x < smallest r = smallest l

prop_functor_law_id t = fmap id t == t

prop_functor_law_comp t = fmap (a.b) t == fmap a (fmap b t)
  where
    a = (+1)
    b = (*2)

prop_size xs = (length . nub) xs == size  (treeify xs)

prop_contains [] = True
prop_contains xs = contains (head xs) (treeify xs)

treeify :: (Ord a) => [a] -> Tree a
treeify = foldr insert Empty
