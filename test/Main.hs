module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Yawn.Application (start)

main :: IO ()
main = do
  serverThread <- forkIO $ start "www"
  -- run tests in here
  killThread serverThread
