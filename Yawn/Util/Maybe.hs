module Yawn.Util.Maybe (
  liftIOMaybe,
  liftIOMaybe_
) where

liftIOMaybe :: b -> (a -> IO (b)) -> IO (Maybe a) -> IO (b)
liftIOMaybe d f m = m >>= \m' -> case m' of
  Nothing -> return d 
  Just y -> f y

liftIOMaybe_ :: (a -> IO ()) -> IO (Maybe a) -> IO ()
liftIOMaybe_ = liftIOMaybe () 
