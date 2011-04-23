module Yawn.Util (
  split,
  concatWith
) where

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f xs  = let (y, ys) = break f xs in y : split f (dropWhile f ys) 

concatWith :: Show a => String -> [a] -> String
concatWith c xs = foldl (\a b -> a ++ c ++ (show b)) "" xs
