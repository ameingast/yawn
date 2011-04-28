module Yawn.Util.Maybe where

liftIOMaybe :: b -> (a -> IO (b)) -> IO (Maybe a) -> IO (b)
liftIOMaybe d f m = m >>= \m' -> case m' of
  Nothing -> return d 
  Just y -> f y

liftIOMaybe_ :: (a -> IO ()) -> IO (Maybe a) -> IO ()
liftIOMaybe_ = liftIOMaybe () 

fromIOMaybe :: IO (Maybe b) -> IO (Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
fromIOMaybe d m f = m >>= \m' -> case m' of
  Nothing -> d
  Just y -> f y

fromIOMaybe_ :: IO (Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
fromIOMaybe_ = fromIOMaybe (return Nothing)
