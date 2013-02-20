module Main (
  main,
  run
) where

import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)
import Yawn.Application as Yawn

main :: IO ()
main = getArgs >>= \a -> case a of
  [] -> putStrLn usage >> exitWith (ExitFailure 1)
  (dir:_) -> run dir

run :: FilePath -> IO ()
run = Yawn.start

usage :: String
usage = "yawn <base directory>"
