module Yawn.Util.List (
  split,
  concatWith,
  endsWith
) where

import List (isSuffixOf)

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f xs  = let (y, ys) = break f xs in y : split f (dropWhile f ys) 

concatWith :: Show a => String -> [a] -> String
concatWith c xs = foldl (\a b -> a ++ c ++ (show b)) "" xs

endsWith :: String -> [String] -> Bool
endsWith s tokens = any (\x -> List.isSuffixOf x s) tokens
