module Yawn.Util.Maybe (
  liftIOMaybe,
  liftIOMaybe_
) where

liftIOMaybe :: a -> (a -> IO (a)) -> IO (Maybe a) -> IO (a)
liftIOMaybe d f m = m >>= \m' -> case m' of
  Nothing -> return d 
  Just y -> f y

liftIOMaybe_ :: (a -> IO ()) -> IO (Maybe a) -> IO ()
liftIOMaybe_ f m = m >>= \m' -> case m' of 
  Nothing -> return ()
  Just y -> f y
