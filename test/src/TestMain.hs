module Main (
  main
) where

import Test.HUnit (runTestTT)
import Yawn.Test.Common (withServer)
import qualified Yawn.Test.BlackBox.ParserTest as ParserTest (tests)

main :: IO ()
main = withServer "www" $ do
  runTestTT ParserTest.tests 
  return ()
