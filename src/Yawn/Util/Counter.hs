module Yawn.Util.Counter (
  Counter,
  makeCounter,
  incCounter,
  readCounter
) where

import Control.Concurrent (MVar, modifyMVar, newMVar, readMVar)

data Counter = CreateCounter {
  incCounter  :: IO (Int),
  readCounter :: IO (Int)
}

makeCounter :: Int -> IO (Counter)
makeCounter k = newMVar k >>= makeCounter'

makeCounter' :: MVar (Int) -> IO (Counter)
makeCounter' l = return $ CreateCounter inc' (readMVar l)
  where inc' = modifyMVar l (\x -> return (x + 1, x))
