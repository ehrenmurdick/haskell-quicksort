module SortTest (sortTest) where

import Data.List ( (\\)
                 , sort
                 )
import Test.QuickCheck
import Lib ( qsort )

sortTest = do
  quickCheck (prop_idempotent :: [Float] -> Bool)
  quickCheck (prop_minimum :: [Int] -> Property)
  quickCheck (prop_maximum :: [Int] -> Property)
  quickCheck (prop_ordered :: [Int] -> Bool)
  quickCheck (prop_permutation :: [Int] -> Bool)
  quickCheck (prop_append :: [Int] -> [Int] -> Property)
  quickCheck (prop_sort_model :: [Int] -> Bool)

prop_idempotent xs = qsort (qsort xs) == qsort xs

prop_minimum xs = not (null xs) ==> (head $ qsort xs) == minimum xs

prop_maximum xs = not (null xs) ==> (head . reverse $ qsort xs) == maximum xs

prop_ordered xs = ordered (qsort xs)
  where
    ordered (x:y:xs) = x<=y && prop_ordered xs
    ordered _ = True

prop_permutation xs = permutation xs (qsort xs)
  where
    permutation xs ys = null (xs \\ ys) && null (ys \\ xs)

prop_append xs ys =
  not (null xs) ==>
  not (null ys) ==>
    head (qsort (xs ++ ys)) == min (minimum xs) (minimum ys)

prop_sort_model xs = sort xs == qsort xs
