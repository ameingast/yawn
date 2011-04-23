module Yawn.Util (
  split
) where

split :: (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split f xs  = let (y, ys) = break f xs in y : split f (dropWhile f ys) 

