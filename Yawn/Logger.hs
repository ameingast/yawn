module Yawn.Logger(
  info,
  debug,
  err
) where

info :: Show a => a -> IO (a)
info s = do
    putStrLn $ "[I] " ++ show s
    return s

debug :: Show a => a -> IO (a)
debug s = do
  putStrLn $ "[D] " ++ show s
  return s

err :: Show a => a -> IO (a)
err s = do
  putStrLn $ "[E] " ++ show s
  return s
